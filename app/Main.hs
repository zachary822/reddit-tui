{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State qualified as MS
import Control.Monad.Trans.State qualified as S
import Crypto.Random
import Data.Aeson hiding (Success)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Lib.Oauth2
import Lib.Reddit
import Lib.Utils
import System.Directory
import Web.Scotty (scotty)
import Web.Scotty qualified as Scotty

import Brick
import Brick.AttrMap qualified as A
import Brick.BChan
import Brick.Widgets.Border qualified as B
import Brick.Widgets.List qualified as L
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events

import Debug.Trace

data CustomEvent = GetPosts | GetPostsResult [Link] (Cursor String) deriving (Show)
data Name = Posts | Post | Comments deriving (Eq, Ord, Show)

type LinkList = L.List Name Link

data AppState = AppState
  { postsState :: LinkList
  , postsCursor :: Cursor String
  , commentState :: LinkList
  , commentCursor :: Cursor String
  , tokenState :: String
  , focusedName :: Name
  , appBChan :: BChan CustomEvent
  }

notSymbol :: Char -> Bool
notSymbol = (/= OtherSymbol) . generalCategory

linkWidget :: Link -> Widget n
linkWidget l = case destUrl l of
  Nothing -> w
  Just u -> hyperlink (T.pack u) w
 where
  w =
    txt "* "
      <+> withAttr (attrName "subreddit") (str $ "(/r/" <> subreddit l <> ") ")
      <+> (txt . T.filter notSymbol $ title l)

renderPostWidget :: Link -> Widget Name
renderPostWidget e =
  reportExtent Post $ case mt of
    Just t -> viewport Post Vertical . txtWrap $ t
    Nothing -> txtWrap $ fromMaybe (T.pack $ url e) (T.pack <$> destUrl e)
 where
  mt = selfText e >>= renderPostBody

renderCommentWidget :: Bool -> Widget Name
renderCommentWidget focused =
  reportExtent Comments $
    B.hBorderWithLabel (renderFocused focused (txt "Comments"))
      <=> ( viewport Comments Vertical $
              txt "yay!"
          )

renderFocused :: Bool -> Widget n -> Widget n
renderFocused focused w =
  if focused then withAttr (attrName "focused") w else w

drawUI :: AppState -> [Widget Name]
drawUI st = [postLayer, ui]
 where
  label = txt "Item " <+> cur <+> txt " of " <+> total
  cur = case L.listSelected $ postsState st of
    Nothing -> txt "-"
    Just i -> str (show $ i + 1)
  total = str . show . Vec.length . L.listElements $ postsState st
  fn = focusedName st
  box =
    B.borderWithLabel label $
      withVScrollBars OnRight $
        clickable Posts $
          L.renderList listDrawElement (fn == Posts) $
            postsState st
  helpText = txt "Press Q to exit"
  ui = vBox [box, helpText]
  postLayer =
    case fn of
      Posts -> emptyWidget
      _ ->
        withVScrollBars OnRight . joinBorders $
          ( maybe
              emptyWidget
              ( \(_, e) ->
                  ( B.borderWithLabel (renderFocused (fn == Post) (txt . T.filter notSymbol . title $ e)) $
                      renderPostWidget e
                        <=> renderCommentWidget (fn == Comments)
                  )
              )
              $ L.listSelectedElement (postsState st)
          )

listDrawElement :: Bool -> Link -> Widget Name
listDrawElement sel a =
  let w = linkWidget a
   in if sel
        then withAttr L.listSelectedAttr w
        else w

loadNextPage :: Event -> EventM Name AppState ()
loadNextPage e = do
  st <- MS.get
  nl <- nestEventM' (postsState st) $ L.handleListEventVi L.handleListEvent e
  let total = Vec.length . L.listElements $ nl
  let curr = L.listSelected nl
  let end = case curr of
        Nothing -> True
        Just n -> n + 1 == total
  if end
    then liftIO $ void $ writeBChanNonBlocking (appBChan st) GetPosts
    else return ()
  MS.put st{postsState = nl}

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent (AppEvent GetPosts) = do
  st <- MS.get
  let bchan = appBChan st
  liftIO $
    void $
      async
        ( do
            (posts, cursor) <- S.runStateT (getNextPosts $ tokenState st) (postsCursor st)
            writeBChan bchan (GetPostsResult posts cursor)
        )
appEvent (AppEvent (GetPostsResult posts cursor)) = do
  st <- MS.get
  let nl = (postsState st)
  let curr = L.listSelected nl <|> Just 0
  MS.put
    st
      { postsState = (L.listReplace (L.listElements nl <> Vec.fromList posts) curr nl)
      , postsCursor = cursor
      }
appEvent (MouseDown Posts BLeft [] (Location (_, y))) = do
  st <- MS.get
  let l = (postsState st)
  nl <- nestEventM' l (MS.modify $ L.listMoveTo y)
  MS.put $ st{postsState = nl}
appEvent (VtyEvent e) = do
  st <- MS.get
  case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> MS.put (st{focusedName = Posts})
    EvKey key [] -> do
      let fn = focusedName st
      let vp = viewportScroll fn
      -- TODO scrolling is scuffed
      case fn of
        Posts ->
          ( case key of
              KEnter -> do
                vScrollToBeginning $ viewportScroll Post
                vScrollToBeginning $ viewportScroll Comments
                (MS.put (st{focusedName = Post}))
              _ -> loadNextPage e
          )
        _ ->
          ( case key of
              KChar '\t' ->
                ( MS.put
                    ( st
                        { focusedName =
                            ( case fn of
                                Comments -> Post
                                Post -> Comments
                                _ -> fn
                            )
                        }
                    )
                )
              KUp -> vScrollBy vp (-1)
              KDown -> vScrollBy vp 1
              KPageUp -> vScrollPage vp Up
              KPageDown -> vScrollPage vp Down
              KHome -> vScrollToBeginning vp
              KEnd -> vScrollToEnd vp
              _ -> return ()
          )
    _ -> loadNextPage e
appEvent _ = return ()

customApp :: App AppState CustomEvent Name
customApp =
  App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = do
        vty <- getVtyHandle
        liftIO $ V.setMode (V.outputIface vty) V.Mouse True
        bchan <- appBChan <$> MS.get

        let output = V.outputIface vty
        liftIO $ do
          (_, h) <- V.displayBounds output
          print h
          replicateM_ (ceiling $ (fromIntegral h / 25 :: Double) :: Int) (writeBChanNonBlocking bchan GetPosts)
    , appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.white)
    , (attrName "focused", V.black `on` V.white)
    , (attrName "subreddit", style V.bold)
    ]

oauth :: Oauth2
oauth =
  Oauth2
    { cliendId = "uZXqaoPoTM6rcxDWjO5rhA"
    , redirectUri = "http://localhost:3000/oauth2/callback"
    , duration = Permanent
    , scopes = ["submit", "save", "read", "history", "subscribe"]
    }

getToken :: FilePath -> IO (Either String RedditToken)
getToken p = eitherDecodeFileStrict' p

getNextPosts :: (MonadThrow m, MonadIO m) => String -> S.StateT (Cursor String) m [Link]
getNextPosts token = do
  cursor <- S.get
  resp <- redditGetEndpoint token "/" cursor []
  S.put (after resp)
  return $ children resp

main :: IO ()
main = do
  path <- getXdgDirectory XdgConfig "reddit-tui/"
  createDirectoryIfMissing False path

  let tokenPath = path <> "refresh_token"

  eitherToken <- getToken tokenPath

  case eitherToken of
    Right token -> do
      putStrLn "Starting..."

      t <- access_token <$> redditAccessToken oauth (refresh_token token)

      bchan <- newBChan 10

      let initialState =
            AppState
              { postsState = L.list Posts Vec.empty 1
              , postsCursor = NoCursor
              , commentState = L.list Comments Vec.empty 1
              , commentCursor = NoCursor
              , tokenState = t
              , focusedName = Posts
              , appBChan = bchan
              }
      let buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty

      void $ customMain initialVty buildVty (Just bchan) customApp initialState

    -- links <- (redditGetEndpoint t ("/comments/13tjc39") NoCursor [("showtitle", Just "0")] :: IO Value)
    -- BS.putStr $ BS.toStrict $ encode links
    Left _ -> do
      chan <- (newTChanIO :: IO (TChan TokenStatus))

      withAsync
        ( do
            rb <- (getRandomBytes 16 :: IO BS.ByteString)
            let st = C8.unpack . B16.encode . BS.toStrict . toLazyByteString $ byteString rb
            -- prints login link to terminal
            putStrLn $ redditOauth oauth st

            scotty 3000 $ do
              Scotty.get "/oauth2/callback" (oauthCallback oauth st chan tokenPath)
        )
        ( \_ -> do
            status <- atomically $ readTChan chan
            putStrLn $ show status

            case status of
              Success -> threadDelay 1000000
              Fail -> error "Token fetching went terribly."
        )

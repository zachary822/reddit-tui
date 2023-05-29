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
import Control.Monad.Trans.Maybe
import Data.Bits (Bits ((.|.)))
import Data.List (intersperse)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events

data CustomEvent
  = GetPosts
  | GetPostsResult [Link] (Cursor String)
  | GetComments String
  | GetCommentsResult [Link]
  deriving (Show)
data Name
  = PostsName
  | PostName
  | CommentsName
  deriving (Eq, Ord, Show)

type LinkList = L.List Name Link

data AppState = AppState
  { postsState :: LinkList
  , postsCursor :: Cursor String
  , commentState :: [Link]
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
      <+> renderVotes (pUpVote l) (pDownVote l)
      <+> withAttr (attrName "subreddit") (str $ "(/r/" <> postSubreddit l <> ") ")
      <+> (txt . T.filter notSymbol $ title l)

renderPostWidget :: Link -> Widget Name
renderPostWidget e =
  case mt of
    Just t -> viewport PostName Vertical $ u <=> txtWrap t
    Nothing -> reportExtent PostName . txtWrap $ fromMaybe (T.pack $ url e) (T.pack <$> destUrl e)
 where
  u = padBottom (Pad 1) . str $ url e
  mt = selfText e >>= renderHtml

renderCommentWidget :: Bool -> [Link] -> Widget Name
renderCommentWidget focused list =
  reportExtent CommentsName $
    B.borderWithLabel
      (renderFocused focused (txt "Comments"))
      ( viewport CommentsName Vertical $
          if length list > 0 then (vBox $ map commentListDrawElement list) else txt "Fetching..."
      )

renderFocused :: Bool -> Widget n -> Widget n
renderFocused focused w =
  if focused then withAttr (attrName "focused") w else w

renderVotes :: (Show a, Show b) => a -> b -> Widget n
renderVotes u d =
  txt "("
    <+> withAttr (attrName "upvote") (str $ show $ u)
    <+> txt "|"
    <+> withAttr (attrName "downvote") (str $ show $ d)
    <+> txt ")"

postListDrawElement :: Bool -> Link -> Widget Name
postListDrawElement sel a =
  let w = linkWidget a
   in if sel
        then withAttr L.listSelectedAttr w
        else w

commentListDrawElement :: Link -> Widget Name
commentListDrawElement a =
  txt "* " <+> w
 where
  w = case a of
    Comment{commentBody = b, cUpVote = u, cDownVote = d} ->
      renderVotes u d
        <+> maybe emptyWidget txt (b >>= renderHtml)
    More{} -> txt "more..."
    _ -> txt ""

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
        clickable PostsName $
          L.renderList postListDrawElement (fn == PostsName) $
            postsState st
  helpText = txt "Press Q to exit"
  ui = vBox [box, helpText]
  postLayer =
    case fn of
      PostsName -> emptyWidget
      _ ->
        withVScrollBars OnRight . joinBorders $
          ( maybe
              emptyWidget
              ( \(_, e) ->
                  ( B.borderWithLabel
                      ( renderFocused
                          (fn == PostName)
                          (renderVotes (pUpVote e) (pDownVote e) <+> (txt . (" " <>) . T.filter notSymbol . title $ e))
                      )
                      (renderPostWidget e)
                      <=> renderCommentWidget (fn == CommentsName) (commentState st)
                  )
              )
              $ L.listSelectedElement (postsState st)
          )

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
appEvent (AppEvent (GetComments cid)) = do
  st <- MS.get
  let bchan = appBChan st
  liftIO $
    void $
      async
        ( do
            comments <- S.evalStateT (getComments (tokenState st) cid) NoCursor
            writeBChan bchan (GetCommentsResult comments)
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
appEvent (AppEvent (GetCommentsResult comments)) = do
  st <- MS.get
  MS.put
    st
      { commentState = comments
      }
appEvent (MouseDown PostsName BLeft [] (Location (_, y))) = do
  st <- MS.get
  let l = (postsState st)
  nl <- nestEventM' l (MS.modify $ L.listMoveTo y)
  MS.put $ st{postsState = nl}
appEvent (VtyEvent e) = do
  st <- MS.get
  case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> MS.put (st{focusedName = PostsName})
    EvKey key [] -> do
      let fn = focusedName st
      let vp = viewportScroll fn
      -- TODO scrolling is scuffed
      case fn of
        PostsName ->
          ( case key of
              KEnter -> do
                vScrollToBeginning $ viewportScroll PostName
                vScrollToBeginning $ viewportScroll CommentsName

                MS.put (st{focusedName = PostName, commentState = []})
                let bchan = appBChan st
                void $ runMaybeT $ do
                  (_, el) <- hoistMaybe . L.listSelectedElement $ (postsState st)
                  case el of
                    Post{postId = pid} -> do
                      liftIO $ writeBChan bchan (GetComments pid)
                    _ -> return ()
              _ -> loadNextPage e
          )
        _ ->
          ( case key of
              KChar '\t' ->
                ( MS.put
                    ( st
                        { focusedName =
                            ( case fn of
                                CommentsName -> PostName
                                PostName -> CommentsName
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
        liftIO $ void $ writeBChanNonBlocking bchan GetPosts
    , appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.white)
    , (attrName "focused", V.black `on` V.white)
    , (attrName "subreddit", style V.bold)
    , (attrName "upvote", V.defAttr `V.withForeColor` V.red `V.withStyle` V.bold)
    , (attrName "downvote", V.defAttr `V.withForeColor` V.blue `V.withStyle` (V.bold .|. V.dim))
    ]

oauth :: Oauth2
oauth =
  Oauth2
    { cliendId = "uZXqaoPoTM6rcxDWjO5rhA"
    , redirectUri = "http://localhost:3000/oauth2/callback"
    , duration = Permanent
    , scopes = ["submit", "save", "read", "history", "subscribe"]
    }

getNextPosts :: (MonadThrow m, MonadIO m) => String -> S.StateT (Cursor String) m [Link]
getNextPosts token = do
  cursor <- S.get
  resp <- redditGetEndpoint token "/" cursor []
  S.put (after resp)
  return $ children resp

getComments :: (MonadThrow m, MonadIO m) => String -> String -> S.StateT (Cursor String) m [Link]
getComments token cid = do
  (_, comments) <-
    ( redditGetEndpoint token ("/comments/" <> cid) NoCursor [("limit", Just "200"), ("sort", Just "top")] ::
        (MonadThrow m, MonadIO m) => m (Listing, Listing)
      )
  return $ children comments

getMoreComments :: (MonadThrow m, MonadIO m) => String -> String -> [String] -> S.StateT (Cursor String) m [Link]
getMoreComments token name cids = do
  moreComments <-
    ( redditGetEndpoint
        token
        "/api/morechildren"
        NoCursor
        ( [ ("children", Just (C8.pack $ concat $ intersperse "," cids))
          , ("link_id", Just (C8.pack name))
          , ("api_type", Just "json")
          ]
        )
      )
  return $ things moreComments

main :: IO ()
main = do
  path <- getXdgDirectory XdgConfig "reddit-tui/"
  createDirectoryIfMissing False path

  let tokenPath = path <> "refresh_token"

  eitherToken <- getToken tokenPath

  case eitherToken of
    Right token -> do
      t <- access_token <$> redditAccessToken oauth (refresh_token token)

      bchan <- newBChan 1

      let initialState =
            AppState
              { postsState = L.list PostsName Vec.empty 1
              , postsCursor = NoCursor
              , commentState = []
              , tokenState = t
              , focusedName = PostsName
              , appBChan = bchan
              }
      let buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty

      void $ customMain initialVty buildVty (Just bchan) customApp initialState
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
      main

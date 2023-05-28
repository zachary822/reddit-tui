{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import Data.ByteString as BS (ByteString, toStrict)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Char
import Data.Text qualified as T
import Lib.Oauth2
import Lib.Reddit
import System.Directory
import Web.Scotty (scotty)
import Web.Scotty qualified as Scotty

import Brick
import Brick.AttrMap qualified as A
import Brick.Widgets.Border qualified as B
import Brick.Widgets.List qualified as L
import Control.Applicative
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events
import Text.Pandoc.Class
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.Markdown

import Control.Monad.IO.Class
import Debug.Trace

data Name = Posts | Post | Comments deriving (Eq, Ord, Show)
type LinkList = L.List Name Link

data AppState = AppState
  { cursorState :: Cursor String
  , listState :: LinkList
  , tokenState :: String
  , focusedName :: Name
  }

notSymbol :: Char -> Bool
notSymbol = (/= OtherSymbol) . generalCategory

linkWidget :: Link -> Widget n
linkWidget l = case destUrl l of
  Nothing -> w
  Just u -> hyperlink (T.pack u) w
 where
  w = txt "* " <+> str ("(/r/" <> subreddit l <> ") ") <+> (txt . T.filter notSymbol $ title l)

renderPost :: T.Text -> Either PandocError T.Text
renderPost e = runPure $ readHtml def e >>= writeMarkdown def

padPostContent :: Widget n -> Widget n
padPostContent = padLeft (Pad 1) . padRight Max . padTop (Pad 1) . padBottom (Pad 1)

renderFocused :: Bool -> Widget n -> Widget n
renderFocused focused w =
  if focused then withAttr (attrName "focused") w else w

drawUI :: AppState -> [Widget Name]
drawUI st = [postLayer, ui]
 where
  label = txt "Item " <+> cur <+> txt " of " <+> total
  cur = case L.listSelected $ listState st of
    Nothing -> txt "-"
    Just i -> str (show $ i + 1)
  total = str . show . Vec.length . L.listElements $ listState st
  fn = focusedName st
  box =
    B.borderWithLabel label $
      withVScrollBars OnRight $
        L.renderList listDrawElement (fn == Posts) $
          listState st
  helpText = txt "Press Q to exit"
  ui = vBox [box, helpText]
  postLayer =
    if fn == Post || fn == Comments
      then
        ( case L.listSelectedElement (listState st) of
            Nothing -> emptyWidget
            Just (_, e) ->
              withVScrollBars OnRight . joinBorders $
                vBox
                  [ ( B.borderWithLabel
                        (renderFocused (fn == Post) (txt . T.filter notSymbol . title $ e))
                        $ viewport Post Vertical
                        $ (txtWrap $ fromMaybe (T.pack $ url e) ((fromRight "" . renderPost <$> selfText e) <|> (T.pack <$> destUrl e)))
                    )
                  , ( B.borderWithLabel
                        (renderFocused (fn == Comments) (txt "Comments"))
                        $ viewport Comments Vertical
                        $ txt "yay!"
                    )
                  ]
        )
      else emptyWidget

listDrawElement :: Bool -> Link -> Widget Name
listDrawElement sel a =
  let w = linkWidget a
   in if sel
        then withAttr L.listSelectedAttr w
        else w

loadNextPage :: Event -> EventM Name AppState ()
loadNextPage e = do
  st <- MS.get
  nl <- nestEventM' (listState st) $ L.handleListEventVi L.handleListEvent e
  let total = Vec.length . L.listElements $ nl
  let curr = L.listSelected nl
  let end = case curr of
        Nothing -> True
        Just n -> n + 1 == total
  if end
    then
      ( do
          (posts, cursor) <- S.runStateT (getNextPosts $ tokenState st) (cursorState st)
          MS.put
            st
              { listState = (L.listReplace (L.listElements nl <> Vec.fromList posts) curr nl)
              , cursorState = cursor
              }
      )
    else MS.put st{listState = nl}

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent (VtyEvent e) = do
  st <- MS.get
  case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc [] -> MS.put (st{focusedName = Posts})
    EvKey key [] -> do
      let fn = focusedName st
      -- TODO scrolling is scuffed
      case fn of
        Posts ->
          ( case key of
              KEnter -> (MS.put (st{focusedName = Post}))
              _ -> loadNextPage e
          )
        Post ->
          ( case key of
              KChar '\t' -> (MS.put (st{focusedName = Comments}))
              KUp -> vScrollBy (viewportScroll fn) (-1)
              KDown -> vScrollBy (viewportScroll fn) 1
              KPageUp -> vScrollPage (viewportScroll fn) Up
              KPageDown -> vScrollPage (viewportScroll fn) Down
              _ -> return ()
          )
        Comments ->
          ( case key of
              KChar '\t' -> (MS.put (st{focusedName = Post}))
              KUp -> vScrollBy (viewportScroll fn) (-1)
              KDown -> vScrollBy (viewportScroll fn) 1
              KPageUp -> vScrollPage (viewportScroll fn) Up
              KPageDown -> vScrollPage (viewportScroll fn) Down
              _ -> return ()
          )
    _ -> loadNextPage e
appEvent _ = return ()

customApp :: App AppState e Name
customApp =
  App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = do
        vty <- getVtyHandle
        liftIO $ V.setMode (V.outputIface vty) V.Mouse True
    , appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.white)
    , (attrName "focused", V.black `on` V.white)
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
  resp <- redditGetEndpoint token "/" cursor
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
      t <- access_token <$> redditAccessToken oauth (refresh_token token)

      (posts, cursor) <- S.runStateT (getNextPosts t) NoCursor

      let initialState =
            AppState
              { listState = (L.list Posts (Vec.fromList posts) 1)
              , cursorState = cursor
              , tokenState = t
              , focusedName = Posts
              }

      void $ defaultMain customApp initialState
    Left _ -> do
      chan <- (newTChanIO :: IO (TChan TokenStatus))

      withAsync
        ( do
            rb <- (getRandomBytes 16 :: IO ByteString)
            let st = C8.unpack . B16.encode . toStrict . toLazyByteString $ byteString rb
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

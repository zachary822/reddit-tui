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
import Control.Monad.Trans.State
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
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events

import Control.Monad.IO.Class
import Debug.Trace

data Name = Posts | SBClick ClickableScrollbarElement Name deriving (Eq, Ord, Show)
type LinkList = L.List Name Link

data AppState = AppState
  { cursorState :: Cursor String
  , listState :: LinkList
  , tokenState :: String
  }

notSymbol :: Char -> Bool
notSymbol = (/= OtherSymbol) . generalCategory

linkWidget :: Link -> Widget n
linkWidget l = case destUrl l of
  Nothing -> w
  Just u -> hyperlink (T.pack u) w
 where
  w = txtWrap . T.filter notSymbol $ "* " <> title l

drawUI :: AppState -> [Widget Name]
drawUI l = [postLayer, ui]
 where
  label = txt "Item " <+> cur <+> txt " of " <+> total
  cur = case L.listSelected $ listState l of
    Nothing -> txt "-"
    Just i -> str (show $ i + 1)
  total = str . show . Vec.length . L.listElements $ listState l
  box =
    reportExtent Posts $
      B.borderWithLabel label $
        withVScrollBars OnRight $
          L.renderList listDrawElement True $
            listState l
  helpText = txt "Press Q to exit"
  ui = vBox [box, helpText]

  -- postLayer = relativeTo Posts (Location (2, 1)) $ B.border $ txt "yay"
  postLayer = relativeTo Posts (Location (2, 1)) emptyWidget

listDrawElement :: Bool -> Link -> Widget Name
listDrawElement sel a =
  let w = linkWidget a
   in if sel
        then withAttr L.listSelectedAttr w
        else w

loadNextPage :: Event -> EventM Name AppState ()
loadNextPage e = do
  st <- MS.get
  nl <- nestEventM' (listState st) $ L.handleListEvent e
  let total = Vec.length . L.listElements $ nl
  let curr = L.listSelected nl
  let end = case curr of
        Nothing -> True
        Just n -> n + 1 == total
  if end
    then
      ( do
          (posts, cursor) <- runStateT (getNextPosts $ tokenState st) (cursorState st)
          MS.put
            st
              { listState = (L.listReplace (L.listElements nl <> Vec.fromList posts) curr nl)
              , cursorState = cursor
              }
      )
    else MS.put st{listState = nl}

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent (VtyEvent e) =
  case e of
    EvKey (KChar 'q') [] -> halt
    _ -> loadNextPage e

-- (posts, cursor) <- runStateT (getNextPosts $ tokenState st) (cursorState st)
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

getNextPosts :: (MonadThrow m, MonadIO m) => String -> StateT (Cursor String) m [Link]
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

      (posts, cursor) <- runStateT (getNextPosts t) NoCursor

      let initialState =
            AppState
              { listState = (L.list Posts (Vec.fromList posts) 1)
              , cursorState = cursor
              , tokenState = t
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

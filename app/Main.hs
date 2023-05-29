{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
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
import Lib.Api
import Lib.Reddit.Oauth2
import Lib.Reddit.Types
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
  | PostBodyName
  | CommentsName
  deriving (Eq, Ord, Show)

type LinkList = L.List Name Link

data AppState = AppState
  { postsState :: LinkList
  , postsCursor :: Cursor String
  , commentState :: [Link]
  , tokenState :: String
  , appBChan :: BChan CustomEvent
  , showPost :: Bool
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
      <+> (txt . T.filter notSymbol $ postTitle l)

renderPostWidget :: Link -> Widget Name
renderPostWidget e =
  reportExtent PostBodyName $ case mt of
    Just t -> u <=> txt t
    Nothing -> txtWrap $ fromMaybe (T.pack $ url e) (T.pack <$> destUrl e)
 where
  u = padBottom (Pad 1) . str $ url e
  mt = selfText e >>= renderHtml

renderCommentWidget :: [Link] -> Widget Name
renderCommentWidget list =
  reportExtent CommentsName $
    B.hBorderWithLabel (txt "Comments")
      <=> ( if length list > 0 then (vBox $ map commentListDrawElement list) else txt "Fetching..."
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

drawPostsUI :: AppState -> Widget Name
drawPostsUI st = vBox [box, helpText]
 where
  label = txt "Item " <+> cur <+> txt " of " <+> total
  cur = case L.listSelected $ postsState st of
    Nothing -> txt "-"
    Just i -> str (show $ i + 1)
  total = str . show . Vec.length . L.listElements $ postsState st
  box =
    B.borderWithLabel label $
      withVScrollBars OnRight $
        L.renderList postListDrawElement True $
          postsState st
  helpText = txt "Press Q to exit"

drawPostUI :: AppState -> Widget Name
drawPostUI st =
  reportExtent CommentsName
    . withVScrollBars OnRight
    . joinBorders
    $ ( maybe
          emptyWidget
          renderSelected
          $ L.listSelectedElement (postsState st)
      )
 where
  renderSelected (_, e) =
    B.borderWithLabel
      (renderVotes (pUpVote e) (pDownVote e) <+> (txt . (" " <>) . T.filter notSymbol . postTitle $ e))
      . viewport PostName Vertical
      $ (renderPostWidget e)
        <=> renderCommentWidget (commentState st)

drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
 where
  ui =
    if not $ showPost st
      then drawPostsUI st
      else drawPostUI st

loadNextPage :: EventM Name AppState ()
loadNextPage = do
  st <- MS.get
  let nl = postsState st
  let total = Vec.length . L.listElements $ nl
  let curr = L.listSelected nl
  let end = case curr of
        Nothing -> True
        Just n -> n + 1 == total
  if end
    then liftIO $ void $ writeBChanNonBlocking (appBChan st) GetPosts
    else return ()
  MS.put st{postsState = nl}

handleNestedPostsState :: EventM n LinkList b -> EventM n AppState AppState
handleNestedPostsState e = do
  st <- MS.get
  let l = (postsState st)
  nl <- nestEventM' l e
  let nst = st{postsState = nl}
  MS.put nst
  return nst

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
appEvent (AppEvent (GetCommentsResult comments)) =
  MS.modify $ \st -> st{commentState = comments}
appEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
appEvent (VtyEvent e) = do
  st <- MS.get
  let sp = showPost st
  case e of
    EvKey KEsc [] -> MS.modify $ \s -> (s{showPost = False})
    EvKey key []
      | sp ->
          ( do
              let vp = viewportScroll PostName
              case key of
                KUp -> vScrollBy vp (-1)
                KDown -> vScrollBy vp 1
                KPageUp -> vScrollPage vp Up
                KPageDown -> vScrollPage vp Down
                KHome -> vScrollToBeginning vp
                KEnd -> vScrollToEnd vp
                _ -> return ()
          )
      | not sp ->
          ( case key of
              KEnter -> do
                vScrollToBeginning $ viewportScroll PostName

                MS.modify $ \s -> s{showPost = True, commentState = []}
                let bchan = appBChan st
                void $ runMaybeT $ do
                  (_, el) <- hoistMaybe . L.listSelectedElement $ (postsState st)
                  case el of
                    Post{postId = pid} -> do
                      liftIO $ writeBChan bchan (GetComments pid)
                    _ -> return ()
              _ -> do
                void $ handleNestedPostsState (L.handleListEventVi L.handleListEvent e)
                loadNextPage
          )
    _ -> do
      void $ handleNestedPostsState (L.handleListEventVi L.handleListEvent e)
      loadNextPage
appEvent _ = return ()

oauth :: Oauth2
oauth =
  Oauth2
    { cliendId = "uZXqaoPoTM6rcxDWjO5rhA"
    , redirectUri = "http://localhost:3000/oauth2/callback"
    , duration = Permanent
    , scopes = ["submit", "save", "read", "history", "subscribe", "mysubreddits"]
    }

customApp :: App AppState CustomEvent Name
customApp =
  App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = do
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
              , appBChan = bchan
              , showPost = False
              }

      let buildVty = V.mkVty =<< V.standardIOConfig
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

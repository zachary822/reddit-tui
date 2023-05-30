{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, withAsync)
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
import Data.CaseInsensitive (original)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Lib.Api
import Lib.Reddit.Oauth2
import Lib.Reddit.Types
import Lib.Utils
import System.Directory
import Text.Printf (PrintfArg, printf)
import Web.Scotty (scotty)
import Web.Scotty qualified as Scotty

import Brick
import Brick.AttrMap qualified as A
import Brick.BChan
import Brick.Widgets.Border qualified as B
import Brick.Widgets.List qualified as L
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Data.Bits (Bits ((.|.)))
import Data.List (sort)
import Data.Time (TimeZone, getCurrentTimeZone)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events

data CustomEvent
  = GetPosts
  | GetPostsResult [Link] (Cursor String)
  | GetSubredditsResult [Link]
  | GetComments String
  | GetCommentsResult [Link]
  deriving (Show)
data Name
  = PostsName
  | PostName
  | PostBodyName
  | CommentsName
  | SubredditsName
  deriving (Eq, Ord, Show)

type LinkList = L.List Name Link

data AppState = AppState
  { postsState :: LinkList
  , postsCursor :: Cursor String
  , commentState :: [Link]
  , tokenState :: String
  , appBChan :: BChan CustomEvent
  , showPost :: Bool
  , showSubreddits :: Bool
  , subredditState :: LinkList
  , currentSubreddit :: Link
  , localTz :: TimeZone
  }

linkWidget :: Link -> Widget n
linkWidget l = case destUrl l of
  Nothing -> w
  Just u -> hyperlink (T.pack u) w
 where
  w =
    renderVotes (postScore l)
      <+> withAttr (attrName "subreddit") (str $ "(/r/" <> postSubreddit l <> ") ")
      <+> (txt $ postTitle l)

renderPostWidget :: TimeZone -> Link -> Widget Name
renderPostWidget tz e =
  reportExtent PostBodyName $
    header <=> case mt of
      Just t -> (padBottom (Pad 1) . strWrap $ url e) <=> txtWrap t
      Nothing -> txtWrap $ fromMaybe (T.pack $ url e) (T.pack <$> destUrl e)
 where
  uwidget = withAttr (attrName "username") $ str ("/u/" <> (postAuthor e) <> " ")
  twidget = str $ formatTimestamp tz (postTimestamp e)
  header = uwidget <+> twidget
  mt = renderHtml <=< selfText $ e

renderCommentWidget :: TimeZone -> [Link] -> Widget Name
renderCommentWidget tz list =
  B.hBorderWithLabel (txt "Comments")
    <=> ( if length list > 0
            then (vBox $ map (commentListDrawElement tz) list)
            else txt "Fetching..."
        )

renderFocused :: Bool -> Widget n -> Widget n
renderFocused focused w =
  if focused then withAttr (attrName "focused") w else w

renderVotes :: (PrintfArg t, Ord t, Num t) => t -> Widget n
renderVotes s =
  txt "("
    <+> attr (str $ printf "%5d" s)
    <+> txt ")"
 where
  attr =
    if s >= 0
      then withAttr (attrName "upvote")
      else withAttr (attrName "downvote")

postListDrawElement :: Bool -> Link -> Widget Name
postListDrawElement sel p =
  let w = linkWidget p
   in if sel
        then withAttr L.listSelectedAttr w
        else w

commentListDrawElement :: TimeZone -> Link -> Widget Name
commentListDrawElement tz comment = w
 where
  w = case comment of
    Comment{commentBody = b, commentScore = sc, commentAuthor = a, commentTimestamp = t, replies = rs} ->
      ( renderVotes sc
          <+> (withAttr (attrName "username") $ str (" /u/" <> a <> " "))
          <+> (withAttr (attrName "timestamp") $ str (formatTimestamp tz t))
      )
        <=> maybe emptyWidget txt (b >>= renderHtml)
        <=> ( if length rs > 0
                then (str "└───" <+> vBox (map (commentListDrawElement tz) rs))
                else emptyWidget
            )
    More{} -> txt "more..."
    _ -> emptyWidget

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
  helpText = txt "Press Q to exit; Press S to show subreddits"

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
  ltz = (localTz st)
  renderSelected (_, e) =
    B.borderWithLabel
      (renderVotes (postScore e) <+> (txt . (" " <>) . postTitle $ e))
      . viewport PostName Vertical
      $ (renderPostWidget ltz e)
        <=> renderCommentWidget ltz (commentState st)

drawSubredditListElement :: Bool -> Link -> Widget Name
drawSubredditListElement sel l =
  if sel
    then withAttr L.listSelectedAttr w
    else w
 where
  w = txt (original $ subredditDisplayName l)

drawUI :: AppState -> [Widget Name]
drawUI st = [subreddits, ui]
 where
  subreddits =
    if showSubreddits st
      then
        hLimit 30 $
          B.borderWithLabel (txt "Subreddits") $
            withVScrollBars OnRight $
              L.renderList drawSubredditListElement False $
                (subredditState st)
      else emptyWidget
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
            (posts, cursor) <-
              S.runStateT
                (getNextPosts (tokenState st) (subredditUrl $ currentSubreddit st))
                (postsCursor st)
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
  let c = postsCursor st
  case c of
    NoCursor -> do
      MS.put
        st
          { postsState = (L.listReplace (Vec.fromList posts) (Just 0) nl)
          , postsCursor = cursor
          }
    _ -> do
      let curr = L.listSelected nl <|> Just 0
      MS.put
        st
          { postsState = (L.listReplace (L.listElements nl <> Vec.fromList posts) curr nl)
          , postsCursor = cursor
          }
appEvent (AppEvent (GetCommentsResult comments)) =
  MS.modify $ \st -> st{commentState = comments}
appEvent (AppEvent (GetSubredditsResult srs)) = do
  st <- MS.get
  nsrs <-
    nestEventM'
      (subredditState st)
      (MS.modify $ L.listReplace (defaultSubs <> Vec.fromList (sort srs)) (Just 0))
  MS.put st{subredditState = nsrs}
appEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
appEvent (VtyEvent (EvKey (KChar 's') [])) = MS.modify $ \s -> s{showSubreddits = (not $ showSubreddits s)}
appEvent (VtyEvent e) = do
  st <- MS.get
  let ssub = showSubreddits st
  let sp = showPost st

  if ssub
    then case e of
      EvKey KEnter [] -> do
        maybe
          (return ())
          ( \(_, s) -> do
              let bchan = appBChan st
              MS.put st{currentSubreddit = s, postsCursor = NoCursor}
              liftIO $ writeBChan bchan GetPosts
          )
          (L.listSelectedElement (subredditState st))
        MS.modify $ \s -> s{showSubreddits = False, showPost = False}
      _ -> do
        nl <- nestEventM' (subredditState st) ((L.handleListEventVi L.handleListEvent e))
        MS.put st{subredditState = nl}
    else case e of
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
      _ -> return ()
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
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.white)
    , (attrName "focused", V.black `on` V.white)
    , (attrName "subreddit", V.defAttr `V.withForeColor` V.yellow)
    , (attrName "upvote", V.defAttr `V.withForeColor` V.red `V.withStyle` V.bold)
    , (attrName "downvote", V.defAttr `V.withForeColor` V.blue `V.withStyle` (V.bold .|. V.dim))
    , (attrName "username", V.defAttr `V.withForeColor` V.cyan)
    , (attrName "timestamp", V.defAttr `V.withForeColor` V.color240 63 63 63)
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
      liftIO $
        void $
          async
            ( do
                (posts, cursor) <-
                  S.runStateT
                    (getNextPosts t (subredditUrl $ Vec.head defaultSubs))
                    NoCursor
                writeBChan bchan (GetPostsResult posts cursor)
            )
      liftIO $
        void $
          async
            ( do
                subreddits <- S.evalStateT (getAllSubreddits t) NoCursor
                writeBChan bchan (GetSubredditsResult subreddits)
            )
      tz <- getCurrentTimeZone

      let initialState =
            AppState
              { postsState = L.list PostsName Vec.empty 1
              , postsCursor = NoCursor
              , commentState = []
              , tokenState = t
              , appBChan = bchan
              , showPost = False
              , showSubreddits = False
              , subredditState = L.list SubredditsName Vec.empty 1
              , currentSubreddit = Vec.head defaultSubs
              , localTz = tz
              }

      let buildVty = do
            v <- V.mkVty =<< V.standardIOConfig
            V.setWindowTitle v "Reddit"
            return v
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

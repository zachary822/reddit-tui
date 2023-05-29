{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Api where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State qualified as S
import Data.ByteString.Char8 qualified as C8
import Data.List (intersperse)
import Lib.Reddit.Oauth2
import Lib.Reddit.Types

getNextPosts :: (MonadThrow m, MonadIO m) => String -> S.StateT (Cursor String) m [Link]
getNextPosts token = do
  cursor <- S.get
  resp <- redditGetEndpoint token "/" cursor [("limit", Just "50")]
  S.put (after resp)
  return $ children resp

getComments :: (MonadThrow m, MonadIO m) => String -> String -> S.StateT (Cursor String) m [Link]
getComments token cid = do
  (_, comments) <-
    ( redditGetEndpoint token ("/comments/" <> cid) NoCursor [] ::
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

getSubreddits :: (MonadThrow m, MonadIO m) => String -> S.StateT (Cursor String) m [Link]
getSubreddits token = do
  cursor <- S.get
  resp <-
    redditGetEndpoint
      token
      "/subreddits/mine/subscriber"
      cursor
      [("sr_detail", Just "0"), ("limit", Just "100")]
  let next = after resp
  S.put next

  if next == NoCursor
    then return $ children resp
    else do
      links <- getSubreddits token
      return $ (children resp) <> links

getAllSubreddits :: (MonadThrow m, MonadIO m) => String -> m [Link]
getAllSubreddits token =
  S.evalStateT (getSubreddits token) NoCursor

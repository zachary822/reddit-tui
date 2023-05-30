{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Reddit.Types where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.CaseInsensitive (CI, mk, original)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

instance FromJSON (CI T.Text) where
  parseJSON = withText "CIString" $ \s ->
    return $ mk s

instance ToJSON (CI T.Text) where
  toJSON = String . original

data Link
  = Post
      { postSubreddit :: String
      , postId :: String
      , postAuthor :: String
      , postScore :: Integer
      , postTitle :: T.Text
      , selfText :: Maybe T.Text
      , url :: String
      , destUrl :: Maybe String
      , postTimestamp :: POSIXTime
      }
  | Comment
      { commentSubreddit :: String
      , commentId :: String
      , commentAuthor :: String
      , commentScore :: Integer
      , commentBody :: Maybe T.Text
      , commentTimestamp :: POSIXTime
      , replies :: [Link]
      }
  | More
      { moreName :: String
      , moreIds :: [String]
      }
  | Subreddit
      { subredditId :: String
      , subredditDisplayName :: CI T.Text
      , subredditType :: String
      , subredditUrl :: String
      }
  deriving (Generic, Show, Eq)

instance Ord Link where
  Subreddit{subredditDisplayName = p1} `compare` Subreddit{subredditDisplayName = p2} = p1 `compare` p2
  _ `compare` _ = EQ

instance FromJSON Link where
  parseJSON = withObject "Link" $ \o -> do
    k <- o .: "kind"
    d <- o .: "data"
    case k of
      "more" -> do
        l <- d .: "name"
        cs <- d .: "children"
        return
          More
            { moreName = l
            , moreIds = cs
            }
      "t1" -> do
        s <- d .: "subreddit"
        l <- d .: "id"
        a <- d .: "author"
        sc <- d .: "score"
        b <- d .:? "body_html"
        r <- (d .: "replies" :: Parser Value)
        rs <- case r of
          Object ro -> (ro .: "data") >>= (.: "children")
          _ -> return mempty
        tm <- d .: "created_utc"

        return
          Comment
            { commentSubreddit = s
            , commentId = l
            , commentAuthor = a
            , commentScore = sc
            , commentBody = b
            , commentTimestamp = tm
            , replies = rs
            }
      "t3" -> do
        s <- d .: "subreddit"
        l <- d .: "id"
        a <- d .: "author"
        sc <- d .: "score"
        t <- d .: "title"
        st <- d .:? "selftext_html"
        u <- d .: "url"
        du <- d .:? "url_overridden_by_dest"
        tm <- d .: "created_utc"

        return
          Post
            { postSubreddit = s
            , postId = l
            , postAuthor = a
            , postScore = sc
            , postTitle = t
            , selfText = st
            , url = u
            , destUrl = du
            , postTimestamp = tm
            }
      "t5" -> do
        l <- d .: "id"
        t <- d .: "display_name"
        p <- d .: "subreddit_type"
        u <- d .: "url"

        return
          Subreddit
            { subredditId = l
            , subredditDisplayName = t
            , subredditType = p
            , subredditUrl = u
            }
      _ -> throw (AesonException $ "bad kind: " <> k)

instance ToJSON Link where
  toEncoding = genericToEncoding defaultOptions

data Cursor a = Before a | After a | NoCursor deriving (Generic, Show, Eq)

instance Functor Cursor where
  fmap f (Before a) = Before (f a)
  fmap f (After a) = After (f a)
  fmap _ NoCursor = NoCursor

instance (ToJSON a) => ToJSON (Cursor a) where
  toEncoding = genericToEncoding defaultOptions

data Listing = Listing
  { before :: Cursor String
  , after :: Cursor String
  , children :: [Link]
  }
  deriving (Generic, Show, Eq)

instance FromJSON Listing where
  parseJSON = withObject "Listing" $ \o -> do
    d <- o .: "data"
    b <- d .:? "before"
    a <- d .:? "after"
    cs <- d .: "children"

    return $ Listing (maybe NoCursor Before b) (maybe NoCursor After a) cs

instance ToJSON Listing where
  toEncoding = genericToEncoding defaultOptions

data MoreComments = MoreComments
  { things :: [Link]
  }
  deriving (Generic, Show)

instance FromJSON MoreComments where
  parseJSON =
    withObject "MoreComments" $
      fmap MoreComments . ((.: "json") >=> (.: "data") >=> (.: "things"))

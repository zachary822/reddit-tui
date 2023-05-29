{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Reddit where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics

data Link
  = Post
      { postSubreddit :: String
      , postId :: String
      , postAuthor :: String
      , pUpVote :: Integer
      , pDownVote :: Integer
      , title :: T.Text
      , selfText :: Maybe T.Text
      , url :: String
      , destUrl :: Maybe String
      }
  | Comment
      { commentSubreddit :: String
      , commentId :: String
      , commentAuthor :: String
      , cUpVote :: Integer
      , cDownVote :: Integer
      , commentBody :: Maybe T.Text
      , replies :: [Link]
      }
  | More
      { moreName :: String
      , moreIds :: [String]
      }
  deriving (Generic, Show, Eq)

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
        uv <- d .: "ups"
        dv <- d .: "downs"
        b <- d .:? "body_html"
        r <- (d .: "replies" :: Parser Value)
        rs <- case r of
          Object ro -> (ro .: "data") >>= (.: "children")
          _ -> return mempty
        return
          Comment
            { commentSubreddit = s
            , commentId = l
            , commentAuthor = a
            , cUpVote = uv
            , cDownVote = dv
            , commentBody = b
            , replies = rs
            }
      "t3" -> do
        s <- d .: "subreddit"
        l <- d .: "id"
        a <- d .: "author"
        uv <- d .: "ups"
        dv <- d .: "downs"
        t <- d .: "title"
        st <- d .:? "selftext_html"
        u <- d .: "url"
        du <- d .:? "url_overridden_by_dest"

        return
          Post
            { postSubreddit = s
            , postId = l
            , postAuthor = a
            , pUpVote = uv
            , pDownVote = dv
            , title = t
            , selfText = st
            , url = u
            , destUrl = du
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

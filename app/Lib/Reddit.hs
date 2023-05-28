{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Reddit where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics

data Link
  = Post
      { postSubreddit :: String
      , postId :: String
      , postAuthor :: String
      , title :: T.Text
      , selfText :: Maybe T.Text
      , url :: String
      , destUrl :: Maybe String
      }
  | Comment
      { commentSubreddit :: String
      , commentId :: String
      , commentAuthor :: String
      , commentBody :: Maybe T.Text
      , replies :: [Link]
      }
  | More
      { moreId :: String
      , moreIds :: [String]
      }
  deriving (Generic, Show)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \o -> do
    k <- o .: "kind"
    d <- o .: "data"
    case k of
      "more" -> do
        l <- d .: "id"
        cs <- d .: "children"
        return
          More
            { moreId = l
            , moreIds = cs
            }
      "t1" -> do
        s <- d .: "subreddit"
        l <- d .: "id"
        a <- d .: "author"
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
            , commentBody = b
            , replies = rs
            }
      "t3" -> do
        s <- d .: "subreddit"
        l <- d .: "id"
        a <- d .: "author"
        t <- d .: "title"
        st <- d .:? "selftext_html"
        u <- d .: "url"
        du <- d .:? "url_overridden_by_dest"

        return
          Post
            { postSubreddit = s
            , postId = l
            , postAuthor = a
            , title = t
            , selfText = st
            , url = u
            , destUrl = du
            }
      _ -> throw (AesonException $ "bad kind: " <> k)

data Cursor a = Before a | After a | NoCursor deriving (Show, Eq)

instance Functor Cursor where
  fmap f (Before a) = Before (f a)
  fmap f (After a) = After (f a)
  fmap _ NoCursor = NoCursor

data Listing a = Listing
  { before :: Cursor String
  , after :: Cursor String
  , children :: [a]
  }
  deriving (Generic, Show)

instance (FromJSON a) => FromJSON (Listing a) where
  parseJSON = withObject "Listing" $ \o -> do
    d <- o .: "data"
    b <- d .:? "before"
    a <- d .:? "after"
    cs <- d .: "children"

    return $ Listing (maybe NoCursor Before b) (maybe NoCursor After a) cs

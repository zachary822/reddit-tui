{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Reddit where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics

data Link = Link
  { subreddit :: String
  , linkId :: String
  , author :: String
  , title :: T.Text
  , selfText :: Maybe T.Text
  , url :: String
  , destUrl :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \o -> do
    k <- (o .: "kind" :: Parser String)
    if k /= "t3" then throw (AesonException "kind not 't3'") else return ()

    d <- o .: "data"
    s <- d .: "subreddit"
    l <- d .: "id"
    a <- d .: "author"
    t <- d .: "title"
    st <- d .:? "selftext_html"
    u <- d .: "url"
    du <- d .:? "url_overridden_by_dest"

    return $
      Link
        { subreddit = s
        , linkId = l
        , author = a
        , title = t
        , selfText = st
        , url = u
        , destUrl = du
        }

data Cursor a = Before a | After a | NoCursor deriving (Show, Eq)

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

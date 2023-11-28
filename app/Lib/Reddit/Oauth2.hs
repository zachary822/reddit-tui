{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Reddit.Oauth2 where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson hiding (Success)
import Data.ByteString.Char8 qualified as C8
import GHC.Generics
import Lib.Reddit.Types
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.URI (escapeURIString, isAllowedInURI)
import Web.Scotty (ActionM)
import Web.Scotty qualified as Scotty

data TokenStatus = Success | Fail deriving (Eq, Show)

data RedditToken = RedditToken
  { access_token :: String
  , token_type :: String
  , expires_in :: Int
  , refresh_token :: String
  , scope :: String
  }
  deriving (Generic, Show)

instance FromJSON RedditToken

instance ToJSON RedditToken where
  toEncoding = genericToEncoding defaultOptions

data Duration = Temporary | Permanent deriving (Eq)

instance Show Duration where
  show d = case d of
    Temporary -> "temporary"
    Permanent -> "permanent"

data Oauth2 = Oauth2
  { cliendId :: String
  , redirectUri :: String
  , duration :: Duration
  , scopes :: [String]
  }
  deriving (Show)

escapeURIString' :: String -> String
escapeURIString' = escapeURIString isAllowedInURI

redditOauth :: Oauth2 -> String -> String
redditOauth oauth2 state =
  concat
    [ "https://www.reddit.com/api/v1/authorize?client_id="
    , escapeURIString' $ cliendId oauth2
    , "&response_type=code&state="
    , escapeURIString' state
    , "&redirect_uri="
    , escapeURIString' $ redirectUri oauth2
    , "&duration="
    , show . duration $ oauth2
    , "&scope="
    , escapeURIString' . unwords . scopes $ oauth2
    ]

redditCode :: (MonadThrow m, MonadIO m) => Oauth2 -> String -> m RedditToken
redditCode oauth code = do
  req <- setRequestBasicAuth (C8.pack $ cliendId oauth) "" <$> parseRequest "https://www.reddit.com/api/v1/access_token"

  let req' =
        req
          { method = "POST"
          , requestHeaders = ("User-Agent", "haskell-tui 0.1.0.0") : requestHeaders req
          , requestBody =
              RequestBodyBS $
                C8.pack . concat $
                  [ "grant_type=authorization_code&code="
                  , code
                  , "&redirect_uri="
                  , redirectUri oauth
                  ]
          }

  resp <- httpJSON req'
  return $ getResponseBody resp

redditAccessToken :: (MonadThrow m, MonadIO m) => Oauth2 -> String -> m RedditToken
redditAccessToken oauth refreshToken = do
  req <- setRequestBasicAuth (C8.pack $ cliendId oauth) "" <$> parseRequest "https://www.reddit.com/api/v1/access_token"

  let req' =
        req
          { method = "POST"
          , requestHeaders = ("User-Agent", "haskell-tui 0.1.0.0") : requestHeaders req
          , requestBody = RequestBodyBS ("grant_type=refresh_token&refresh_token=" <> C8.pack refreshToken)
          }

  resp <- httpJSON req'
  return $ getResponseBody resp

redditGetEndpoint :: (MonadThrow m, MonadIO m, FromJSON a) => String -> String -> Cursor String -> Query -> m a
redditGetEndpoint accessToken endpoint cursor opts = do
  req <- parseRequest ("https://oauth.reddit.com" ++ endpoint)
  let req' =
        req
          { method = "GET"
          , requestHeaders =
              ("User-Agent", "haskell-tui 0.1.0.0")
                : ("Authorization", "bearer " <> C8.pack accessToken)
                : requestHeaders req
          , queryString = ""
          }

  let qs =
        ("raw_json", Just "1")
          : ( case cursor of
                NoCursor -> []
                After c -> [("after", Just $ C8.pack c)]
                Before c -> [("before", Just $ C8.pack c)]
            )

  let req'' = addToRequestQueryString (qs <> opts) req'

  resp <- httpJSON req''
  return $ getResponseBody resp

oauthCallback :: Oauth2 -> String -> TChan TokenStatus -> FilePath -> ActionM ()
oauthCallback oauth state chan tokenPath = do
  st <- Scotty.queryParam "state"

  when (st /= state) $
    Scotty.raise "Error: reauthenticate by restarting the app"

  code <- Scotty.queryParam "code"

  result <- redditCode oauth code

  liftIO $ encodeFile tokenPath result

  Scotty.text "You may close this window"
  liftIO $ atomically $ writeTChan chan Success

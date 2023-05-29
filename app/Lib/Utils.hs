module Lib.Utils where

import Control.Exception
import Control.Monad
import Data.Aeson (eitherDecodeFileStrict')
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lib.Reddit.Oauth2 (RedditToken)
import Text.Pandoc (def, readHtml, runPure, writeMarkdown)
import Text.Pandoc.Sources (ToSources)

renderHtml :: (ToSources a) => a -> Maybe Text
renderHtml =
  either
    (const Nothing)
    (Just . toStrict . toLazyText . htmlEncodedText)
    . runPure
    . (writeMarkdown def <=< readHtml def)

getToken :: FilePath -> IO (Either String RedditToken)
getToken p = catch (eitherDecodeFileStrict' p) $ \e -> (return $ Left . show $ (e :: IOException))

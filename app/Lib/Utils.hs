module Lib.Utils where

import Control.Exception
import Data.Aeson (eitherDecodeFileStrict')
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lib.Oauth2 (RedditToken)
import Text.Pandoc (def, readHtml, runPure, writeMarkdown)
import Text.Pandoc.Sources (ToSources)

renderHtml :: (ToSources a) => a -> Maybe Text
renderHtml e =
  case (runPure $ readHtml def e >>= writeMarkdown def) of
    Left _ -> Nothing
    Right t -> Just $ toStrict . toLazyText . htmlEncodedText $ t

getToken :: FilePath -> IO (Either String RedditToken)
getToken p = catch (eitherDecodeFileStrict' p) $ \e -> (return $ Left . show $ (e :: IOException))

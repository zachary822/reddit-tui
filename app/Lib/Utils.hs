module Lib.Utils where

import Control.Exception
import Control.Monad
import Data.Aeson (eitherDecodeFileStrict')
import Data.Text (Text)
import Data.Time (TimeZone, defaultTimeLocale, formatTime, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Lib.Reddit.Oauth2 (RedditToken)
import Text.Pandoc (def, readHtml, runPure, writePlain)
import Text.Pandoc.Sources (ToSources)

renderHtml :: (ToSources a) => a -> Maybe Text
renderHtml =
  either
    (const Nothing)
    Just
    . runPure
    . (writePlain def <=< readHtml def)

getToken :: FilePath -> IO (Either String RedditToken)
getToken p = catch (eitherDecodeFileStrict' p) $ \e -> (return $ Left . show $ (e :: IOException))

formatTimestamp :: TimeZone -> POSIXTime -> String
formatTimestamp tz t = formatTime defaultTimeLocale "%c" . utcToZonedTime tz . posixSecondsToUTCTime $ t

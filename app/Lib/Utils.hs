module Lib.Utils where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson (eitherDecodeFileStrict')
import Data.Text (Text)
import Data.Time (TimeZone, defaultTimeLocale, formatTime, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Lib.Reddit.Oauth2 (RedditToken)
import Network.URI
import System.Process (callProcess)
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

openUrl :: MonadIO m => String -> MaybeT m ()
openUrl url = do
  guard (isURI url)
  uri <- hoistMaybe $ parseURI url
  guard $ (uriScheme uri) == "https:"
  ua <- hoistMaybe $ uriAuthority uri
  guard $ (uriUserInfo ua) == ""
  liftIO $ callProcess "open" [url]

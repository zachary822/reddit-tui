module Lib.Utils where

import Data.Text (Text, stripEnd)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Text.Pandoc (def, readHtml, runPure, writeMarkdown)

rightToMaybe :: Either a1 a2 -> Maybe a2
rightToMaybe e =
  case e of
    Left _ -> Nothing
    Right a -> Just a

renderPostBody :: Text -> Maybe Text
renderPostBody e =
  toStrict . toLazyText . htmlEncodedText . stripEnd
    <$> (rightToMaybe $ runPure $ readHtml def e >>= writeMarkdown def)

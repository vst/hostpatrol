{-# LANGUAGE OverloadedStrings #-}

-- | This module provides auxiliary definitions for textual values.
module Zamazingo.Text where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE


-- $setup
--
-- >>> :set -XOverloadedStrings


-- | Like 'show' but produces 'T.Text'.
tshow :: Show a => a -> T.Text
tshow = T.pack . show


-- | Sanitizes a given string by replacing consecutive whitespace with
-- a single space.
--
-- >>> sanitize ""
-- ""
-- >>> sanitize "  "
-- ""
-- >>> sanitize "  a   c   b  "
-- "a c b"
sanitize :: T.Text -> T.Text
sanitize = T.unwords . T.words


-- | Returns a 'Maybe' of non empty string.
--
-- >>> nonempty ""
-- Nothing
-- >>> nonempty " "
-- Just " "
nonempty :: T.Text -> Maybe T.Text
nonempty "" = Nothing
nonempty x = Just x


-- | Attempts to convert a given lazy bytestring to strict text.
--
-- May throw encoding error, hence the @unsafe@.
unsafeTextFromBL :: BL.ByteString -> T.Text
unsafeTextFromBL = TL.toStrict . TLE.decodeUtf8


-- | Converts a given strict text to lazy bytestring.
blFromText :: T.Text -> BL.ByteString
blFromText = TLE.encodeUtf8 . TL.fromStrict

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides auxiliary definitions for working with
-- date/time values.
module Zamazingo.Time where

import qualified Autodocodec as ADC
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as Time.Iso8601
import GHC.Generics (Generic)


-- | Data definition for date/time information.
--
-- The reason that this type exists is to avoid the legacy
-- 'Time.UTCTime' type schema typing.
newtype DateTime = DateTime
  { _unDateTime :: Time.UTCTime
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DateTime)


-- | 'ADC.HasCodec' instance for 'DateTime'.
--
-- >>> :set -XTypeApplications
-- >>> import Autodocodec as ADC
-- >>> import Autodocodec.Schema as ADC.Schema
--
-- >>> let now = DateTime (read "2021-09-01 12:00:00 UTC" :: Time.UTCTime)
-- >>> Aeson.encode now
-- "\"2021-09-01T12:00:00Z\""
-- >>> Aeson.decode @DateTime (Aeson.encode now)
-- Just (DateTime {_unDateTime = 2021-09-01 12:00:00 UTC})
--
-- >>> Aeson.encode (ADC.Schema.jsonSchemaViaCodec @DateTime)
-- "{\"$comment\":\"Date/time in ISO8601 format.\",\"type\":\"string\"}"
instance ADC.HasCodec DateTime where
  codec =
    ADC.bimapCodec (fmap DateTime . parseIso8601) (iso8601 . _unDateTime) ADC.textCodec ADC.<?> "Date/time in ISO8601 format."


-- | Returns current date/time information.
--
-- > getNow
-- DateTime {_unDateTime = 2024-04-18 00:48:15.956143715 UTC}
getNow :: MonadIO m => m DateTime
getNow = liftIO (DateTime <$> Time.getCurrentTime)


-- | Parses ISO8601 date/time string.
parseIso8601 :: T.Text -> Either String Time.UTCTime
parseIso8601 t =
  maybe (Left err) pure (Time.Iso8601.formatParseM Time.Iso8601.iso8601Format (T.unpack t))
  where
    err = "ISO8601 date/time parse error: " <> show t


-- | Parses ISO8601 date/time string in 'MonadFail' context.
parseIso8601M :: MonadFail m => T.Text -> m Time.UTCTime
parseIso8601M =
  either fail pure . parseIso8601


-- | Formats 'Time.UTCTime' into an ISO8601 date/time string.
iso8601 :: Time.UTCTime -> T.Text
iso8601 = T.pack . Time.Iso8601.formatShow Time.Iso8601.iso8601Format

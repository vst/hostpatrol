{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines public data and type definitions to represent
-- application configuration.
module Lhp.Config where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Lhp.Types as Types


-- | Data definition for application configuration.
data Config = Config
  { _configHosts :: ![Types.Host]
  , _configKnownSshKeys :: ![T.Text]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Config)


instance ADC.HasCodec Config where
  codec =
    _codec ADC.<?> "Application Configuration"
    where
      _codec =
        ADC.object "Config" $
          Config
            <$> ADC.optionalFieldWithDefault "hosts" [] "List of hosts." ADC..= _configHosts
            <*> ADC.optionalFieldWithDefault "knownSshKeys" [] "List of hosts." ADC..= _configKnownSshKeys


-- | Attempts to read a configuration file and return 'Config'.
--
-- Note that this function errors out with 'error' if something goes
-- wrong (TODO).
readConfigFile :: FilePath -> IO Config
readConfigFile fp = do
  res <- Yaml.decodeFileEither fp
  case res of
    Left err -> error (show err)
    Right sv -> pure sv

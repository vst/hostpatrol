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
import Zamazingo.Ssh (SshConfig)


-- | Data definition for application configuration.
data Config = Config
  { _configHosts :: ![HostSpec]
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
            <*> ADC.optionalFieldWithDefault "knownSshKeys" [] "Known SSH public keys for all hosts." ADC..= _configKnownSshKeys


-- | Data definition for host specification.
data HostSpec = HostSpec
  { _hostSpecName :: !T.Text
  , _hostSpecSsh :: !(Maybe SshConfig)
  , _hostSpecId :: !(Maybe T.Text)
  , _hostSpecUrl :: !(Maybe T.Text)
  , _hostSpecTags :: ![T.Text]
  , _hostSpecData :: !Aeson.Value
  , _hostSpecKnownSshKeys :: ![T.Text]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec HostSpec)


instance ADC.HasCodec HostSpec where
  codec =
    _codec ADC.<?> "Host Specification"
    where
      _codec =
        ADC.object "HostSpec" $
          HostSpec
            <$> ADC.requiredField "name" "Name of the host." ADC..= _hostSpecName
            <*> ADC.optionalField "ssh" "SSH configuration." ADC..= _hostSpecSsh
            <*> ADC.optionalField "id" "External identifier of the host." ADC..= _hostSpecId
            <*> ADC.optionalField "url" "URL to external host information." ADC..= _hostSpecUrl
            <*> ADC.optionalFieldWithDefault "tags" [] "Arbitrary tags for the host." ADC..= _hostSpecTags
            <*> ADC.optionalFieldWithDefault "data" Aeson.Null "Arbitrary data for the host." ADC..= _hostSpecData
            <*> ADC.optionalFieldWithDefault "knownSshKeys" [] "Known SSH public keys for the host." ADC..= _hostSpecKnownSshKeys


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

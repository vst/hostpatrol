{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides definitions to retrieve and parse remote
-- host information and produce host report.
module Lhp.Remote where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedStringFile)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as S
import qualified Data.Text as T
import Lhp.Types (Report (_reportSystemdServices))
import qualified Lhp.Types as Types
import Text.Read (readEither)
import qualified Zamazingo.Ssh as Z.Ssh
import qualified Zamazingo.Text as Z.Text


-- * Report


-- | Attempts to retrieve remote host information and produce a host
-- report.
compileReport
  :: MonadIO m
  => MonadError LhpError m
  => Types.Host
  -> m Types.Report
compileReport h@Types.Host {..} = do
  kvs <- (++) <$> _fetchHostInfo _hostName <*> _fetchHostCloudInfo _hostName
  let _reportHost = h
  _reportCloud <- _mkCloud _hostName kvs
  _reportHardware <- _mkHardware _hostName kvs
  _reportKernel <- _mkKernel _hostName kvs
  _reportDistribution <- _mkDistribution _hostName kvs
  _reportDockerContainers <- _fetchHostDockerContainers _hostName
  _reportSshAuthorizedKeys <- _fetchHostSshAuthorizedKeys _hostName
  _reportSystemdServices <- _fetchHostSystemdServices _hostName
  _reportSystemdTimers <- _fetchHostSystemdTimers _hostName
  pure Types.Report {..}


-- * Errors


-- | Data definition for error(s) which can be thrown while retrieving
-- remote host information and producing host report.
data LhpError
  = LhpErrorSsh Z.Ssh.Destination Z.Ssh.SshError
  | LhpErrorParse Z.Ssh.Destination T.Text
  deriving (Eq, Show)


instance Aeson.ToJSON LhpError where
  toJSON (LhpErrorSsh h err) = Aeson.object [("type", "ssh"), "host" Aeson..= h, "error" Aeson..= err]
  toJSON (LhpErrorParse h err) = Aeson.object [("type", "parse"), "host" Aeson..= h, "error" Aeson..= err]


-- * Internal


-- | Attempts to retrieve remote host information and return it as a
-- list of key/value tuples.
_fetchHostInfo
  :: MonadIO m
  => MonadError LhpError m
  => Z.Ssh.Destination
  -> m [(T.Text, T.Text)]
_fetchHostInfo h =
  parseKVs <$> _toSshError h (Z.Ssh.runScript h $(embedStringFile "src/scripts/info.sh") ["bash"])


-- | Attempts to retrieve remote host cloud information and return it
-- as a list of key/value tuples.
_fetchHostCloudInfo
  :: MonadIO m
  => MonadError LhpError m
  => Z.Ssh.Destination
  -> m [(T.Text, T.Text)]
_fetchHostCloudInfo h =
  parseKVs <$> _toSshError h (Z.Ssh.runScript h $(embedStringFile "src/scripts/cloud.sh") ["bash"])


-- | Attempts to retrieve remote host docker containers information and return it.
--
-- Returns 'Nothing' if remote host is not identified as a Docker
-- host, a list of rudimentary Docker container information otherwise.
_fetchHostDockerContainers
  :: MonadIO m
  => MonadError LhpError m
  => Z.Ssh.Destination
  -> m (Maybe [Types.DockerContainer])
_fetchHostDockerContainers h =
  (Just <$> (prog >>= _parseDockerContainers)) `catchError` const (pure Nothing)
  where
    prog = _toSshError h (Z.Ssh.runScript h $(embedStringFile "src/scripts/docker-containers.sh") ["bash"])
    _parseDockerContainers b =
      case ACD.eitherDecode (ACD.list _jsonDecoderDockerContainer) b of
        Left err -> throwError (LhpErrorParse h ("Error while parsing containers information: " <> T.pack err))
        Right sv -> pure sv


-- | Attempts to find and return all SSH authorized keys on the remote
-- host.
_fetchHostSshAuthorizedKeys
  :: MonadIO m
  => MonadError LhpError m
  => Z.Ssh.Destination
  -> m [T.Text]
_fetchHostSshAuthorizedKeys h =
  filter (not . T.null) . fmap T.strip . T.lines . Z.Text.unsafeTextFromBL <$> prog
  where
    prog = _toSshError h (Z.Ssh.runScript h $(embedStringFile "src/scripts/ssh-keys.sh") ["bash"])


-- | Attempts to find and return all systemd services on the remote
-- host.
_fetchHostSystemdServices
  :: MonadIO m
  => MonadError LhpError m
  => Z.Ssh.Destination
  -> m [T.Text]
_fetchHostSystemdServices h =
  filter (not . T.null) . fmap T.strip . T.lines . Z.Text.unsafeTextFromBL <$> prog
  where
    prog = _toSshError h (Z.Ssh.runScript h $(embedStringFile "src/scripts/systemd-services.sh") ["bash"])


-- | Attempts to find and return all systemd timers on the remote
-- host.
_fetchHostSystemdTimers
  :: MonadIO m
  => MonadError LhpError m
  => Z.Ssh.Destination
  -> m [T.Text]
_fetchHostSystemdTimers h =
  filter (not . T.null) . fmap T.strip . T.lines . Z.Text.unsafeTextFromBL <$> prog
  where
    prog = _toSshError h (Z.Ssh.runScript h $(embedStringFile "src/scripts/systemd-timers.sh") ["bash"])


-- | Smart constructor for remote host cloud information.
_mkCloud
  :: MonadError LhpError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Cloud
_mkCloud h kvs =
  _toParseError h $ do
    _cloudName <- fromMaybe "UNKNOWN" <$> _findParse pure "LHP_CLOUD_NAME" kvs
    _cloudHostId <- _findParse pure "LHP_CLOUD_ID" kvs
    _cloudHostType <- _findParse pure "LHP_CLOUD_TYPE" kvs
    _cloudHostRegion <- _findParse pure "LHP_CLOUD_REGION" kvs
    _cloudHostAvailabilityZone <- _findParse pure "LHP_CLOUD_AVAILABILITY_ZONE" kvs
    _cloudHostLocalHostname <- _findParse pure "LHP_CLOUD_LOCAL_HOSTNAME" kvs
    _cloudHostLocalAddress <- _findParse pure "LHP_CLOUD_LOCAL_ADDRESS" kvs
    _cloudHostRemoteHostname <- _findParse pure "LHP_CLOUD_PUBLIC_HOSTNAME" kvs
    _cloudHostRemoteAddress <- _findParse pure "LHP_CLOUD_PUBLIC_ADDRESS" kvs
    _cloudHostReservedAddress <- _findParse pure "LHP_CLOUD_RESERVED_ADDRESS" kvs
    pure Types.Cloud {..}


-- | Smart constructor for remote host rudimentary hardware
-- information.
_mkHardware
  :: MonadError LhpError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Hardware
_mkHardware h kvs =
  _toParseError h $ do
    _hardwareCpuCount <- _getParse _parseRead "LHP_HW_CPU" kvs
    _hardwareRamTotal <- _getParse (fmap (_roundS 2 . _toGB) . _parseRead) "LHP_HW_RAM" kvs
    _hardwareDiskRoot <- _getParse (fmap (_roundS 2 . _toGB) . _parseRead) "LHP_HW_DISK" kvs
    pure Types.Hardware {..}


-- | Smart constructor for remote host kernel information.
_mkKernel
  :: MonadError LhpError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Kernel
_mkKernel h kvs =
  _toParseError h $ do
    _kernelNode <- _getParse pure "LHP_KERNEL_NODE" kvs
    _kernelName <- _getParse pure "LHP_KERNEL_NAME" kvs
    _kernelRelease <- _getParse pure "LHP_KERNEL_RELEASE" kvs
    _kernelVersion <- _getParse pure "LHP_KERNEL_VERSION" kvs
    _kernelMachine <- _getParse pure "LHP_KERNEL_MACHINE" kvs
    _kernelOs <- _getParse pure "LHP_KERNEL_OS" kvs
    pure Types.Kernel {..}


-- | Smart constructor for remote host distribution information.
_mkDistribution
  :: MonadError LhpError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Distribution
_mkDistribution h kvs =
  _toParseError h $ do
    _distributionId <- _getParse pure "LHP_DISTRO_ID" kvs
    _distributionName <- _getParse pure "LHP_DISTRO_NAME" kvs
    _distributionVersion <- _getParse pure "LHP_DISTRO_VERSION" kvs
    _distributionRelease <- _getParse pure "LHP_DISTRO_VERSION_ID" kvs
    _distributionCodename <- _findParse pure "LHP_DISTRO_VERSION_CODENAME" kvs
    _distributionDescription <- _getParse pure "LHP_DISTRO_PRETTY_NAME" kvs
    pure Types.Distribution {..}


-- | Attempts to parse a list of key/value tuples formatted in the
-- @key=value@ format.
parseKVs :: BL.ByteString -> [(T.Text, T.Text)]
parseKVs =
  fmap _parseKV . T.lines . Z.Text.unsafeTextFromBL


-- | Attempts to parse and return key/value tuple formatted in the
-- key=value format.
--
-- Value can be unqouted, single-quoted or double-quoted.
_parseKV :: T.Text -> (T.Text, T.Text)
_parseKV =
  fmap (_parseValue . T.drop 1) . T.break (== '=')


-- | Parses a quoted or unquoted value.
--
-- >>> :set -XOverloadedStrings
-- >>> _parseValue ""
-- ""
-- >>> _parseValue "''"
-- ""
-- >>> _parseValue "\"\""
-- ""
-- >>> _parseValue "a"
-- "a"
-- >>> _parseValue "'a'"
-- "a"
-- >>> _parseValue "\"a\""
-- "a"
_parseValue :: T.Text -> T.Text
_parseValue v =
  case T.uncons v of -- TODO: Check closing quotation mark, too.
    Nothing -> v
    Just ('\'', _) -> T.replace "\\'" "'" . T.dropEnd 1 . T.drop 1 $ v
    Just ('\"', _) -> T.replace "\\\"" "\"" . T.dropEnd 1 . T.drop 1 $ v
    _ -> v


-- | 'ACD.Decoder' for 'Types.DockerContainertainer'.
--
-- This is used to parse `docker inspect` result collection elements.
_jsonDecoderDockerContainer :: ACD.Decoder Types.DockerContainer
_jsonDecoderDockerContainer =
  Types.DockerContainer
    <$> ACD.key "Id" ACD.text
    <*> (T.dropWhile (== '/') <$> ACD.key "Name" ACD.text)
    <*> ACD.at ["Config", "Image"] ACD.text
    <*> ACD.key "Created" ACD.utcTime
    <*> ((==) True <$> ACD.at ["State", "Running"] ACD.bool)


-- | Attempts to find a given key and returns its value parsed with
-- the given parser within a given list of key/value pairs.
_findParse
  :: MonadError T.Text m
  => (T.Text -> m a)
  -> T.Text
  -> [(T.Text, T.Text)]
  -> m (Maybe a)
_findParse vp k kvs =
  case snd <$> List.find ((==) k . fst) kvs of
    Nothing -> pure Nothing
    Just sv -> Just <$> vp sv


-- | Attempts to find a given key and returns its value parsed with
-- the given parser within a given list of key/value pairs.
--
-- Similar to '_findParse' but throws error if key is not found.
_getParse
  :: MonadError T.Text m
  => (T.Text -> m a)
  -> T.Text
  -> [(T.Text, T.Text)]
  -> m a
_getParse vp k kvs =
  _findParse vp k kvs >>= maybe err pure
  where
    err = throwError ("Variable " <> k <> " is not found.")


-- | Rounds the given number to given number of decimal points.
--
-- >>> _roundS 2 2.105
-- 2.1
-- >>> _roundS 2 2.115
-- 2.12
_roundS :: Int -> S.Scientific -> S.Scientific
_roundS n =
  read . S.formatScientific S.Generic (Just n)


-- | Converts the value to GBs.
_toGB :: S.Scientific -> S.Scientific
_toGB s =
  s / (1024 * 1024)


-- | (Borrowed from mtl v2.3.1)
_modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
_modifyError f m =
  runExceptT m >>= either (throwError . f) pure


-- | 'readEither' that works with 'T.Text' in a @'MonadError'
-- 'T.Text'@ context
_parseRead
  :: MonadError T.Text m
  => Read a
  => T.Text
  -> m a
_parseRead =
  either (throwError . T.pack) pure . readEither . T.unpack


-- | Lifts @'ExceptT' 'T.Text'@ to @'MonadError' 'LhpError'@ with
-- parse error.
_toParseError
  :: MonadError LhpError m
  => Z.Ssh.Destination
  -> ExceptT T.Text m a
  -> m a
_toParseError h =
  _modifyError (LhpErrorParse h)


-- | Lifts @'ExceptT' 'Z.Ssh.SshError'@ to @'MonadError' 'LhpError'@
-- with SSH error.
_toSshError
  :: MonadError LhpError m
  => Z.Ssh.Destination
  -> ExceptT Z.Ssh.SshError m a
  -> m a
_toSshError h =
  _modifyError (LhpErrorSsh h)

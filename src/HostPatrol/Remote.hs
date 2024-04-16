{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides definitions to retrieve and parse remote
-- host information and produce host report.
module HostPatrol.Remote where

import qualified Control.Concurrent.Async.Pool as Async.Pool
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import Data.FileEmbed (embedStringFile)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified HostPatrol.Config as Config
import qualified HostPatrol.Meta as Meta
import qualified HostPatrol.Types as Types
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as TP
import Text.Read (readEither)
import qualified Zamazingo.Ssh as Z.Ssh
import qualified Zamazingo.Text as Z.Text


-- * Report


-- | Attempts to compile host patrol report for a given configuration.
compileReport
  :: MonadError HostPatrolError m
  => MonadIO m
  => Int
  -> Config.Config
  -> m Types.Report
compileReport par Config.Config {..} = do
  now <- liftIO Time.getCurrentTime
  (_errs, _reportHosts) <- liftIO (compileHostReportsIO par _configHosts)
  _reportKnownSshKeys <- concat <$> mapM parseSshPublicKeys _configKnownSshKeys
  let _reportMeta =
        Types.ReportMeta
          { _reportMetaVersion = Meta._buildInfoVersion Meta.buildInfo
          , _reportMetaBuildTag = Meta._buildInfoGitTag Meta.buildInfo
          , _reportMetaBuildHash = Meta._buildInfoGitHash Meta.buildInfo
          , _reportMetaTimestamp = now
          }
  pure Types.Report {..}


-- | Attempts to compile host reports for a given list of host
-- specifications and parallelism.
--
-- If any host report fails to compile, it will be returned in the
-- error list, otherwise in the host report list.
compileHostReportsIO
  :: Int
  -> [Config.HostSpec]
  -> IO ([HostPatrolError], [Types.HostReport])
compileHostReportsIO par hs =
  Async.Pool.withTaskGroup par $
    \tg -> partitionEithers <$> Async.Pool.mapConcurrently tg (runExceptT . compileHostReport) hs


-- | Attempts to retrieve remote host information and produce a host
-- report.
compileHostReport
  :: MonadIO m
  => MonadError HostPatrolError m
  => Config.HostSpec
  -> m Types.HostReport
compileHostReport ch = do
  h@Types.Host {..} <- _makeHostFromConfigHostSpec ch
  kvs <- (++) <$> _fetchHostInfo h <*> _fetchHostCloudInfo h
  let _hostReportHost = h
  _hostReportHostname <- _toParseError _hostName $ _getParse pure "HOSTPATROL_GENERAL_HOSTNAME" kvs
  _hostReportTimezone <- _toParseError _hostName $ _getParse pure "HOSTPATROL_GENERAL_TIMEZONE" kvs
  _hostReportCloud <- _mkCloud _hostName kvs
  _hostReportHardware <- _mkHardware _hostName kvs
  _hostReportKernel <- _mkKernel _hostName kvs
  _hostReportDistribution <- _mkDistribution _hostName kvs
  _hostReportDockerContainers <- _fetchHostDockerContainers h
  _hostReportPublicSshHostKeys <- _fetchHostPublicSshHostKeys h >>= fmap concat . mapM parseSshPublicKeys
  _hostReportAuthorizedSshKeys <- _fetchHostAuthorizedSshKeys h >>= fmap concat . mapM parseSshPublicKeys
  _hostReportSystemdServices <- _fetchHostSystemdServices h
  _hostReportSystemdTimers <- _fetchHostSystemdTimers h
  pure Types.HostReport {..}


-- | Consumes a 'Config.HostSpec' and produces a 'Types.Host'.
_makeHostFromConfigHostSpec
  :: MonadError HostPatrolError m
  => MonadIO m
  => Config.HostSpec
  -> m Types.Host
_makeHostFromConfigHostSpec Config.HostSpec {..} =
  let _hostName = _hostSpecName
      _hostSsh = _hostSpecSsh
      _hostId = _hostSpecId
      _hostUrl = _hostSpecUrl
      _hostTags = _hostSpecTags
      _hostData = _hostSpecData
   in do
        _hostKnownSshKeys <- concat <$> mapM parseSshPublicKeys _hostSpecKnownSshKeys
        pure Types.Host {..}


-- * Errors


-- | Data definition for error(s) which can be thrown while retrieving
-- remote host information and producing host report.
data HostPatrolError
  = HostPatrolErrorSsh Z.Ssh.Destination Z.Ssh.SshError
  | HostPatrolErrorParse Z.Ssh.Destination T.Text
  | HostPatrolErrorUnknown T.Text
  deriving (Eq, Show)


instance Aeson.ToJSON HostPatrolError where
  toJSON (HostPatrolErrorSsh h err) = Aeson.object [("type", "ssh"), "host" Aeson..= h, "error" Aeson..= err]
  toJSON (HostPatrolErrorParse h err) = Aeson.object [("type", "parse"), "host" Aeson..= h, "error" Aeson..= err]
  toJSON (HostPatrolErrorUnknown err) = Aeson.object [("type", "unknown"), "error" Aeson..= err]


-- * Internal


-- | Builds 'Z.Ssh.SshConfig' value from 'Types.Host'.
--
-- If the host does not have an SSH configuration, it will default to
-- an SSH configuration with the host name as the destination.
getHostSshConfig :: Types.Host -> Z.Ssh.SshConfig
getHostSshConfig Types.Host {..} =
  fromMaybe Z.Ssh.SshConfig {_sshConfigDestination = _hostName, _sshConfigOptions = []} _hostSsh


-- | Attempts to retrieve remote host information and return it as a
-- list of key/value tuples.
_fetchHostInfo
  :: MonadIO m
  => MonadError HostPatrolError m
  => Types.Host
  -> m [(T.Text, T.Text)]
_fetchHostInfo h@Types.Host {..} =
  parseKVs <$> _toSshError _hostName (Z.Ssh.runScript (getHostSshConfig h) $(embedStringFile "src/scripts/info.sh") ["bash"])


-- | Attempts to retrieve remote host cloud information and return it
-- as a list of key/value tuples.
_fetchHostCloudInfo
  :: MonadIO m
  => MonadError HostPatrolError m
  => Types.Host
  -> m [(T.Text, T.Text)]
_fetchHostCloudInfo h@Types.Host {..} =
  parseKVs <$> _toSshError _hostName (Z.Ssh.runScript (getHostSshConfig h) $(embedStringFile "src/scripts/cloud.sh") ["bash"])


-- | Attempts to retrieve remote host docker containers information and return it.
--
-- Returns 'Nothing' if remote host is not identified as a Docker
-- host, a list of rudimentary Docker container information otherwise.
_fetchHostDockerContainers
  :: MonadIO m
  => MonadError HostPatrolError m
  => Types.Host
  -> m (Maybe [Types.DockerContainer])
_fetchHostDockerContainers h@Types.Host {..} =
  (Just <$> (prog >>= _parseDockerContainers)) `catchError` const (pure Nothing)
  where
    prog = _toSshError _hostName (Z.Ssh.runScript (getHostSshConfig h) $(embedStringFile "src/scripts/docker-containers.sh") ["bash"])
    _parseDockerContainers b =
      case ACD.eitherDecode (ACD.list _jsonDecoderDockerContainer) b of
        Left err -> throwError (HostPatrolErrorParse _hostName ("Error while parsing containers information: " <> T.pack err))
        Right sv -> pure sv


-- | Attempts to find and return all public SSH host keys on the remote
-- host.
_fetchHostPublicSshHostKeys
  :: MonadIO m
  => MonadError HostPatrolError m
  => Types.Host
  -> m [T.Text]
_fetchHostPublicSshHostKeys h@Types.Host {..} =
  filter (not . T.null) . fmap T.strip . T.lines . Z.Text.unsafeTextFromBL <$> prog
  where
    prog = _toSshError _hostName (Z.Ssh.runScript (getHostSshConfig h) $(embedStringFile "src/scripts/ssh-host-keys.sh") ["bash"])


-- | Attempts to find and return all SSH authorized keys on the remote
-- host.
_fetchHostAuthorizedSshKeys
  :: MonadIO m
  => MonadError HostPatrolError m
  => Types.Host
  -> m [T.Text]
_fetchHostAuthorizedSshKeys h@Types.Host {..} =
  filter (not . T.null) . fmap T.strip . T.lines . Z.Text.unsafeTextFromBL <$> prog
  where
    prog = _toSshError _hostName (Z.Ssh.runScript (getHostSshConfig h) $(embedStringFile "src/scripts/ssh-keys.sh") ["bash"])


-- | Attempts to find and return all systemd services on the remote
-- host.
_fetchHostSystemdServices
  :: MonadIO m
  => MonadError HostPatrolError m
  => Types.Host
  -> m [T.Text]
_fetchHostSystemdServices h@Types.Host {..} =
  filter (not . T.null) . fmap T.strip . T.lines . Z.Text.unsafeTextFromBL <$> prog
  where
    prog = _toSshError _hostName (Z.Ssh.runScript (getHostSshConfig h) $(embedStringFile "src/scripts/systemd-services.sh") ["bash"])


-- | Attempts to find and return all systemd timers on the remote
-- host.
_fetchHostSystemdTimers
  :: MonadIO m
  => MonadError HostPatrolError m
  => Types.Host
  -> m [T.Text]
_fetchHostSystemdTimers h@Types.Host {..} =
  filter (not . T.null) . fmap T.strip . T.lines . Z.Text.unsafeTextFromBL <$> prog
  where
    prog = _toSshError _hostName (Z.Ssh.runScript (getHostSshConfig h) $(embedStringFile "src/scripts/systemd-timers.sh") ["bash"])


-- | Smart constructor for remote host cloud information.
_mkCloud
  :: MonadError HostPatrolError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Cloud
_mkCloud h kvs =
  _toParseError h $ do
    _cloudName <- fromMaybe "UNKNOWN" <$> _findParse pure "HOSTPATROL_CLOUD_NAME" kvs
    _cloudHostId <- _findParse pure "HOSTPATROL_CLOUD_ID" kvs
    _cloudHostType <- _findParse pure "HOSTPATROL_CLOUD_TYPE" kvs
    _cloudHostRegion <- _findParse pure "HOSTPATROL_CLOUD_REGION" kvs
    _cloudHostAvailabilityZone <- _findParse pure "HOSTPATROL_CLOUD_AVAILABILITY_ZONE" kvs
    _cloudHostLocalHostname <- _findParse pure "HOSTPATROL_CLOUD_LOCAL_HOSTNAME" kvs
    _cloudHostLocalAddress <- _findParse pure "HOSTPATROL_CLOUD_LOCAL_ADDRESS" kvs
    _cloudHostRemoteHostname <- _findParse pure "HOSTPATROL_CLOUD_PUBLIC_HOSTNAME" kvs
    _cloudHostRemoteAddress <- _findParse pure "HOSTPATROL_CLOUD_PUBLIC_ADDRESS" kvs
    _cloudHostReservedAddress <- _findParse pure "HOSTPATROL_CLOUD_RESERVED_ADDRESS" kvs
    pure Types.Cloud {..}


-- | Smart constructor for remote host rudimentary hardware
-- information.
_mkHardware
  :: MonadError HostPatrolError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Hardware
_mkHardware h kvs =
  _toParseError h $ do
    _hardwareCpuCount <- _getParse _parseRead "HOSTPATROL_HW_CPU" kvs
    _hardwareRamTotal <- _getParse (fmap (_roundS 2 . _toGB) . _parseRead) "HOSTPATROL_HW_RAM" kvs
    _hardwareDiskRoot <- _getParse (fmap (_roundS 2 . _toGB) . _parseRead) "HOSTPATROL_HW_DISK" kvs
    pure Types.Hardware {..}


-- | Smart constructor for remote host kernel information.
_mkKernel
  :: MonadError HostPatrolError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Kernel
_mkKernel h kvs =
  _toParseError h $ do
    _kernelNode <- _getParse pure "HOSTPATROL_KERNEL_NODE" kvs
    _kernelName <- _getParse pure "HOSTPATROL_KERNEL_NAME" kvs
    _kernelRelease <- _getParse pure "HOSTPATROL_KERNEL_RELEASE" kvs
    _kernelVersion <- _getParse pure "HOSTPATROL_KERNEL_VERSION" kvs
    _kernelMachine <- _getParse pure "HOSTPATROL_KERNEL_MACHINE" kvs
    _kernelOs <- _getParse pure "HOSTPATROL_KERNEL_OS" kvs
    pure Types.Kernel {..}


-- | Smart constructor for remote host distribution information.
_mkDistribution
  :: MonadError HostPatrolError m
  => Z.Ssh.Destination
  -> [(T.Text, T.Text)]
  -> m Types.Distribution
_mkDistribution h kvs =
  _toParseError h $ do
    _distributionId <- _getParse pure "HOSTPATROL_DISTRO_ID" kvs
    _distributionName <- _getParse pure "HOSTPATROL_DISTRO_NAME" kvs
    _distributionVersion <- _getParse pure "HOSTPATROL_DISTRO_VERSION" kvs
    _distributionRelease <- _getParse pure "HOSTPATROL_DISTRO_VERSION_ID" kvs
    _distributionCodename <- _findParse pure "HOSTPATROL_DISTRO_VERSION_CODENAME" kvs
    _distributionDescription <- _getParse pure "HOSTPATROL_DISTRO_PRETTY_NAME" kvs
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


-- | Lifts @'ExceptT' 'T.Text'@ to @'MonadError' 'HostPatrolError'@ with
-- parse error.
_toParseError
  :: MonadError HostPatrolError m
  => Z.Ssh.Destination
  -> ExceptT T.Text m a
  -> m a
_toParseError h =
  _modifyError (HostPatrolErrorParse h)


-- | Lifts @'ExceptT' 'Z.Ssh.SshError'@ to @'MonadError' 'HostPatrolError'@
-- with SSH error.
_toSshError
  :: MonadError HostPatrolError m
  => Z.Ssh.Destination
  -> ExceptT Z.Ssh.SshError m a
  -> m a
_toSshError h =
  _modifyError (HostPatrolErrorSsh h)


-- | Creates list of 'Types.SshPublicKey' from given 'T.Text' using @ssh-keygen@.
--
-- If the given 'T.Text' is a GitHub username, it will attempt to
-- fetch keys from GitHub and then parse them using @ssh-keygen@.
--
-- > runExceptT $ parseSshPublicKeys "gh:vst"
-- Right [SshPublicKey {_sshPublicKeyData = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJIQtEmoHu44pUDwX5GEw20JLmfZaI+xVXin74GI396z", _sshPublicKeyType = "ED25519", _sshPublicKeyLength = 256, _sshPublicKeyComment = "gh:vst", _sshPublicKeyFingerprint = "MD5:01:6d:4f:ca:c9:ca:dc:f1:cb:a3:fc:74:8e:34:77:16"},SshPublicKey {_sshPublicKeyData = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3", _sshPublicKeyType = "ED25519", _sshPublicKeyLength = 256, _sshPublicKeyComment = "gh:vst", _sshPublicKeyFingerprint = "MD5:ec:4b:ff:8d:c7:43:a9:ab:16:9f:0d:fa:8f:e2:6f:6c"}]
-- > runExceptT $ parseSshPublicKeys "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3"
-- Right [SshPublicKey {_sshPublicKeyData = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3", _sshPublicKeyType = "ED25519", _sshPublicKeyLength = 256, _sshPublicKeyComment = "no comment", _sshPublicKeyFingerprint = "MD5:ec:4b:ff:8d:c7:43:a9:ab:16:9f:0d:fa:8f:e2:6f:6c"}]
-- > runExceptT $ parseSshPublicKeys "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3 comment"
-- Right [SshPublicKey {_sshPublicKeyData = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3 comment", _sshPublicKeyType = "ED25519", _sshPublicKeyLength = 256, _sshPublicKeyComment = "comment", _sshPublicKeyFingerprint = "MD5:ec:4b:ff:8d:c7:43:a9:ab:16:9f:0d:fa:8f:e2:6f:6c"}]
-- > runExceptT $ parseSshPublicKeys "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3 some more comment"
-- Right [SshPublicKey {_sshPublicKeyData = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3 some more comment", _sshPublicKeyType = "ED25519", _sshPublicKeyLength = 256, _sshPublicKeyComment = "some more comment", _sshPublicKeyFingerprint = "MD5:ec:4b:ff:8d:c7:43:a9:ab:16:9f:0d:fa:8f:e2:6f:6c"}]
-- > runExceptT $ parseSshPublicKeys "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3 some more comment"
-- Right [SshPublicKey {_sshPublicKeyData = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdd2ubdTn5LPsN0zaxylrpkQTW+1Vr/uWQaEQXoGkd3 some more comment", _sshPublicKeyType = "ED25519", _sshPublicKeyLength = 256, _sshPublicKeyComment = "some more comment", _sshPublicKeyFingerprint = "MD5:ec:4b:ff:8d:c7:43:a9:ab:16:9f:0d:fa:8f:e2:6f:6c"}]
parseSshPublicKeys
  :: MonadError HostPatrolError m
  => MonadIO m
  => T.Text
  -> m [Types.SshPublicKey]
parseSshPublicKeys s = do
  let gh = "gh:"
  if T.isPrefixOf gh s
    then do
      let u = T.drop (T.length gh) s
      ks <- listGitHubSshKeys u
      fmap (\x -> x {Types._sshPublicKeyComment = s}) <$> mapM parseSshPublicKey ks
    else List.singleton <$> parseSshPublicKey s


-- | Attempts to create 'Types.SshPublicKey' from given SSH public key
-- represented as 'T.Text' using @ssh-keygen@.
parseSshPublicKey
  :: MonadError HostPatrolError m
  => MonadIO m
  => T.Text
  -> m Types.SshPublicKey
parseSshPublicKey s = do
  (ec, out, err) <- TP.readProcess process
  case ec of
    ExitFailure _ -> throwUnknown (Z.Text.unsafeTextFromBL err <> ". Input was: " <> s)
    ExitSuccess -> case T.words (Z.Text.unsafeTextFromBL out) of
      (l : fp : r) ->
        pure $
          Types.SshPublicKey
            { _sshPublicKeyData = s
            , _sshPublicKeyType = T.init . T.tail $ List.last r
            , _sshPublicKeyLength = read (T.unpack l)
            , _sshPublicKeyComment = T.unwords (filter (not . T.null) (List.init r))
            , _sshPublicKeyFingerprint = fp
            }
      _ -> throwUnknown "Could not parse ssh-keygen output."
  where
    throwUnknown = throwError . HostPatrolErrorUnknown
    stdin = TP.byteStringInput (Z.Text.blFromText s)
    process = TP.setStdin stdin (TP.proc "ssh-keygen" ["-E", "md5", "-l", "-f", "-"])


-- | Attempts to get the list of SSH public keys from GitHub for a
-- given GitHub username.
listGitHubSshKeys
  :: MonadError HostPatrolError m
  => MonadIO m
  => T.Text
  -> m [T.Text]
listGitHubSshKeys u = do
  (ec, out, err) <- TP.readProcess process
  case ec of
    ExitFailure _ ->
      throwUnknown
        ( "Error while retrieving SSH public keys from GitHub for "
            <> u
            <> ". Exit code: "
            <> Z.Text.tshow ec
            <> ". STDOUT: "
            <> Z.Text.unsafeTextFromBL out
            <> ". STDERR: "
            <> Z.Text.unsafeTextFromBL err
        )
    ExitSuccess -> pure (toKeys out)
  where
    throwUnknown = throwError . HostPatrolErrorUnknown
    process = TP.proc "curl" ["-sS", "https://github.com/" <> T.unpack u <> ".keys"]
    toKeys = filter (not . T.null) . T.lines . Z.Text.unsafeTextFromBL

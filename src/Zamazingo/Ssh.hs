{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides definitions to work with remote hosts over
-- SSH.
module Zamazingo.Ssh where

import qualified Autodocodec as ADC
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Path as P
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as TP
import qualified Zamazingo.Text as Z.Text


-- | Type definition for remote SSH host.
type Destination = T.Text


-- | Data definition for SSH configuration.
data SshConfig = SshConfig
  { _sshConfigDestination :: !Destination
  , _sshConfigOptions :: ![T.Text]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec SshConfig)


instance ADC.HasCodec SshConfig where
  codec =
    _codec ADC.<?> "SSH Configuration"
    where
      _codec =
        ADC.object "SshConfig" $
          SshConfig
            <$> ADC.requiredField "destination" "SSH destination." ADC..= _sshConfigDestination
            <*> ADC.optionalFieldWithDefault "options" [] "SSH options." ADC..= _sshConfigOptions


-- | Data definition for errors this module can throw.
data SshError
  = SshErrorConnection SshConfig T.Text
  | SshErrorCommandTimeout SshConfig [T.Text]
  | SshErrorCommand SshConfig [T.Text]
  | SshErrorFileRead SshConfig (P.Path P.Abs P.File)
  | SshErrorMissingFile SshConfig (P.Path P.Abs P.File)
  deriving (Eq, Show)


instance Aeson.ToJSON SshError where
  toJSON (SshErrorConnection d err) =
    Aeson.object [("type", "connection"), "destination" .= d, "error" .= err]
  toJSON (SshErrorCommandTimeout d cmd) =
    Aeson.object [("type", "command-timeout"), "destination" .= d, "command" .= cmd]
  toJSON (SshErrorCommand d cmd) =
    Aeson.object [("type", "command"), "destination" .= d, "command" .= cmd]
  toJSON (SshErrorFileRead d p) =
    Aeson.object [("type", "file-read"), "destination" .= d, "path" .= p]
  toJSON (SshErrorMissingFile d p) =
    Aeson.object [("type", "missing-file"), "destination" .= d, "path" .= p]


sshErrorToText :: SshError -> T.Text
sshErrorToText (SshErrorConnection _ err) = "SSH connection error: " <> T.strip err
sshErrorToText (SshErrorCommandTimeout _ cmd) = "SSH command timed out: " <> T.unwords cmd
sshErrorToText (SshErrorCommand _ cmd) = "SSH command failed: " <> T.unwords cmd
sshErrorToText (SshErrorFileRead _ p) = "Failed to read remote file: " <> T.pack (P.toFilePath p)
sshErrorToText (SshErrorMissingFile _ p) = "Remote file does not exist: " <> T.pack (P.toFilePath p)


-- | Attempts to run a command on the remote and return its stdout.
runCommand
  :: MonadIO m
  => MonadError SshError m
  => SshConfig
  -> [T.Text]
  -> m BL.ByteString
runCommand cfg cmd = do
  (ec, out, _err) <- _runCommand cfg Nothing _sshConfig cmd
  case ec of
    ExitFailure _ -> throwError (SshErrorCommand cfg cmd)
    ExitSuccess -> pure out


-- | Attempts to run a script on the remote and return its stdout.
runScript
  :: MonadIO m
  => MonadError SshError m
  => SshConfig
  -> BL.ByteString
  -> [T.Text]
  -> m BL.ByteString
runScript cfg scr cmd = do
  (ec, out, _err) <- _runCommand cfg (Just scr) _sshConfig cmd
  case ec of
    ExitFailure _ -> throwError (SshErrorCommand cfg cmd)
    ExitSuccess -> pure out


-- | Attempts to read the remote file and return its contents.
readFile
  :: MonadIO m
  => MonadError SshError m
  => SshConfig
  -> P.Path P.Abs P.File
  -> m BL.ByteString
readFile cfg p = do
  exists <- doesFileExist cfg p
  unless exists (throwError (SshErrorMissingFile cfg p))
  (ec, out, _err) <- _runCommand cfg Nothing _sshConfig _cmd
  case ec of
    ExitFailure _ -> throwError (SshErrorFileRead cfg p)
    ExitSuccess -> pure out
  where
    _cmd = ["cat", T.pack (P.toFilePath p)]


-- | Checks if the command exists on remote.
hasCommand
  :: MonadIO m
  => MonadError SshError m
  => SshConfig
  -> T.Text
  -> m Bool
hasCommand cfg cmd = do
  (ec, _out, _err) <- _runCommand cfg Nothing _sshConfig _cmd
  case ec of
    ExitFailure 1 -> pure False
    ExitFailure _ -> throwError (SshErrorCommand cfg _cmd)
    ExitSuccess -> pure True
  where
    _cmd = ["which", cmd]


-- | Checks if the file exists on remote.
doesFileExist
  :: MonadIO m
  => MonadError SshError m
  => SshConfig
  -> P.Path P.Abs P.File
  -> m Bool
doesFileExist cfg p = do
  (ec, _out, _err) <- _runCommand cfg Nothing _sshConfig _cmd
  case ec of
    ExitFailure 2 -> pure False
    ExitFailure _ -> throwError (SshErrorCommand cfg _cmd)
    ExitSuccess -> pure True
  where
    _cmd = ["[ -f \"" <> T.pack (P.toFilePath p) <> "\" ] && exit 0 || exit 2"]


-- | Attempts to run a command on the remote and return its exit code,
-- stdout and stderr.
--
-- If @ssh@ fails (with exit code 255), throws 'SshErrorConnection'
-- error.
--
-- If command is timed-out, throws 'SshErrorCommandTimeout' error.
_runCommand
  :: MonadIO m
  => MonadError SshError m
  => SshConfig
  -> Maybe BL.ByteString
  -> [T.Text]
  -> [T.Text]
  -> m (ExitCode, BL.ByteString, BL.ByteString)
_runCommand cfg@SshConfig {..} mi opts cmd = do
  let stdin = maybe TP.nullStream TP.byteStringInput mi
  (ec, out, err) <- TP.readProcess (TP.setStdin stdin $ TP.proc "timeout" ("10" : "ssh" : _args))
  case ec of
    ExitFailure 124 -> throwError (SshErrorCommandTimeout cfg cmd)
    ExitFailure 255 -> throwError (SshErrorConnection cfg ("Connection failed: " <> Z.Text.unsafeTextFromBL err))
    ExitFailure _ -> pure (ec, out, err)
    ExitSuccess -> pure (ec, out, err)
  where
    _args = fmap T.unpack (opts <> _sshConfigOptions <> [_sshConfigDestination] <> cmd)


-- | SSH config to pass on all our SSH connections.
_sshConfig :: [T.Text]
_sshConfig = ["-o", "ConnectTimeout=5"]

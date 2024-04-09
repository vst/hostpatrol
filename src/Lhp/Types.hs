{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines public data and type definitions to represent
-- a complete lhp report.
module Lhp.Types where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import Data.Int (Int32)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Zamazingo.Ssh (SshConfig)


-- * Report


-- | Data definition for host patrol report.
data Report = Report
  { _reportHosts :: ![HostReport]
  , _reportKnownSshKeys :: ![SshPublicKey]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Report)


instance ADC.HasCodec Report where
  codec =
    _codec ADC.<?> "Host Patrol Report"
    where
      _codec =
        ADC.object "Report" $
          Report
            <$> ADC.requiredField "hosts" "List of host reports." ADC..= _reportHosts
            <*> ADC.requiredField "knownSshKeys" "List of known SSH public keys." ADC..= _reportKnownSshKeys


-- * Host


-- | Data definition for host descriptor.
data Host = Host
  { _hostName :: !T.Text
  , _hostSsh :: !(Maybe SshConfig)
  , _hostId :: !(Maybe T.Text)
  , _hostUrl :: !(Maybe T.Text)
  , _hostTags :: ![T.Text]
  , _hostData :: !Aeson.Value
  , _hostKnownSshKeys :: ![SshPublicKey]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Host)


instance ADC.HasCodec Host where
  codec =
    _codec ADC.<?> "Host Descriptor"
    where
      _codec =
        ADC.object "Host" $
          Host
            <$> ADC.requiredField "name" "Name of the host." ADC..= _hostName
            <*> ADC.optionalField "ssh" "SSH configuration." ADC..= _hostSsh
            <*> ADC.optionalField "id" "External identifier of the host." ADC..= _hostId
            <*> ADC.optionalField "url" "URL to external host information." ADC..= _hostUrl
            <*> ADC.optionalFieldWithDefault "tags" [] "Arbitrary tags for the host." ADC..= _hostTags
            <*> ADC.optionalFieldWithDefault "data" Aeson.Null "Arbitrary data for the host." ADC..= _hostData
            <*> ADC.optionalFieldWithDefault "knownSshKeys" [] "Known SSH public keys for the host." ADC..= _hostKnownSshKeys


-- * Host Report


-- | Data definition for host patrol report.
data HostReport = HostReport
  { _hostReportHost :: !Host
  , _hostReportHostname :: !T.Text
  , _hostReportTimezone :: !T.Text
  , _hostReportCloud :: !Cloud
  , _hostReportHardware :: !Hardware
  , _hostReportKernel :: !Kernel
  , _hostReportDistribution :: !Distribution
  , _hostReportDockerContainers :: !(Maybe [DockerContainer])
  , _hostReportAuthorizedSshKeys :: ![SshPublicKey]
  , _hostReportSystemdServices :: ![T.Text]
  , _hostReportSystemdTimers :: ![T.Text]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec HostReport)


instance ADC.HasCodec HostReport where
  codec =
    _codec ADC.<?> "Host Patrol Report"
    where
      _codec =
        ADC.object "Report" $
          HostReport
            <$> ADC.requiredField "host" "Host descriptor." ADC..= _hostReportHost
            <*> ADC.requiredField "hostname" "Hostname of the host." ADC..= _hostReportHostname
            <*> ADC.requiredField "timezone" "Timezone of the host." ADC..= _hostReportTimezone
            <*> ADC.requiredField "cloud" "Cloud information." ADC..= _hostReportCloud
            <*> ADC.requiredField "hardware" "Hardware information." ADC..= _hostReportHardware
            <*> ADC.requiredField "kernel" "Kernel information." ADC..= _hostReportKernel
            <*> ADC.requiredField "distribution" "Distribution information." ADC..= _hostReportDistribution
            <*> ADC.requiredField "dockerContainers" "List of Docker containers if the host is a Docker host." ADC..= _hostReportDockerContainers
            <*> ADC.requiredField "authorizedSshKeys" "List of authorized SSH public keys found on host." ADC..= _hostReportAuthorizedSshKeys
            <*> ADC.requiredField "systemdServices" "List of systemd services found on host." ADC..= _hostReportSystemdServices
            <*> ADC.requiredField "systemdTimers" "List of systemd timers found on host." ADC..= _hostReportSystemdTimers


-- * Cloud Information


-- | Data definition for host's cloud information.
data Cloud = Cloud
  { _cloudName :: !T.Text
  , _cloudHostId :: !(Maybe T.Text)
  , _cloudHostType :: !(Maybe T.Text)
  , _cloudHostRegion :: !(Maybe T.Text)
  , _cloudHostAvailabilityZone :: !(Maybe T.Text)
  , _cloudHostLocalHostname :: !(Maybe T.Text)
  , _cloudHostLocalAddress :: !(Maybe T.Text)
  , _cloudHostRemoteHostname :: !(Maybe T.Text)
  , _cloudHostRemoteAddress :: !(Maybe T.Text)
  , _cloudHostReservedAddress :: !(Maybe T.Text)
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Cloud)


instance ADC.HasCodec Cloud where
  codec =
    _codec ADC.<?> "Cloud Information"
    where
      _codec =
        ADC.object "Cloud" $
          Cloud
            <$> ADC.requiredField "name" "Cloud name." ADC..= _cloudName
            <*> ADC.requiredField "id" "Host identifier." ADC..= _cloudHostId
            <*> ADC.requiredField "hostType" "Host type." ADC..= _cloudHostType
            <*> ADC.requiredField "hostRegion" "Host region." ADC..= _cloudHostRegion
            <*> ADC.requiredField "hostAvailabilityZone" "Host availability zone." ADC..= _cloudHostAvailabilityZone
            <*> ADC.requiredField "hostLocalHostname" "Local hostname of the host." ADC..= _cloudHostLocalHostname
            <*> ADC.requiredField "hostLocalAddress" "Local address of the host." ADC..= _cloudHostLocalAddress
            <*> ADC.requiredField "hostRemoteHostname" "Remote hostname of the host." ADC..= _cloudHostRemoteHostname
            <*> ADC.requiredField "hostRemoteAddress" "Remote address of the host." ADC..= _cloudHostRemoteAddress
            <*> ADC.requiredField "hostReservedAddress" "Reserved address of the host." ADC..= _cloudHostReservedAddress


-- * Hardware Information


-- | Data definition for host's rudimentary hardware information.
data Hardware = Hardware
  { _hardwareCpuCount :: !Int32 -- :)
  , _hardwareRamTotal :: !Scientific
  , _hardwareDiskRoot :: !Scientific
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Hardware)


instance ADC.HasCodec Hardware where
  codec =
    _codec ADC.<?> "Rudimentary Hardware Information"
    where
      _codec =
        ADC.object "Hardware" $
          Hardware
            <$> ADC.requiredField "cpuCount" "Number of (v)CPU cores." ADC..= _hardwareCpuCount
            <*> ADC.requiredField "ramTotal" "Total RAM (in GB)." ADC..= _hardwareRamTotal
            <*> ADC.requiredField "diskRoot" "Total disk space of root (`/`) filesystem (in GB)." ADC..= _hardwareDiskRoot


-- * Kernel Information


-- | Data definition for host's kernel information.
data Kernel = Kernel
  { _kernelNode :: !T.Text
  , _kernelName :: !T.Text
  , _kernelRelease :: !T.Text
  , _kernelVersion :: !T.Text
  , _kernelMachine :: !T.Text
  , _kernelOs :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Kernel)


instance ADC.HasCodec Kernel where
  codec =
    _codec ADC.<?> "Kernel Information"
    where
      _codec =
        ADC.object "Kernel" $
          Kernel
            <$> ADC.requiredField "node" "Name of the node kernel is running on (uname -n)." ADC..= _kernelNode
            <*> ADC.requiredField "name" "Kernel name (uname -s)." ADC..= _kernelName
            <*> ADC.requiredField "release" "Kernel release (uname -r)." ADC..= _kernelRelease
            <*> ADC.requiredField "version" "Kernel version (uname -v)." ADC..= _kernelVersion
            <*> ADC.requiredField "machine" "Architecture the kernel is running on (uname -m)." ADC..= _kernelMachine
            <*> ADC.requiredField "os" "Operating system the kernel is driving (uname -o)." ADC..= _kernelOs


-- * Distribution Information


-- | Data definition for host's distribution information.
data Distribution = Distribution
  { _distributionId :: !T.Text
  , _distributionName :: !T.Text
  , _distributionVersion :: !T.Text
  , _distributionRelease :: !T.Text
  , _distributionCodename :: !(Maybe T.Text)
  , _distributionDescription :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Distribution)


instance ADC.HasCodec Distribution where
  codec =
    _codec ADC.<?> "Distribution Information"
    where
      _codec =
        ADC.object "Distribution" $
          Distribution
            <$> ADC.requiredField "id" "Distribution ID (cat /etc/os-release | grep 'ID=')." ADC..= _distributionId
            <*> ADC.requiredField "name" "Distribution name (cat /etc/os-release | grep 'NAME='))." ADC..= _distributionName
            <*> ADC.requiredField "version" "Distribution version (cat /etc/os-release | grep 'VERSION=')." ADC..= _distributionVersion
            <*> ADC.requiredField "release" "Distribution release (cat /etc/os-release | grep 'VERSION_ID=')." ADC..= _distributionRelease
            <*> ADC.requiredField "codename" "Distribution codename (cat /etc/os-release | grep 'VERSION_CODENAME=')." ADC..= _distributionCodename
            <*> ADC.requiredField "description" "Distribution description (cat /etc/os-release | grep 'PRETTY_NAME=')." ADC..= _distributionDescription


-- * Docker Container Information


-- | Data definition for Docker container information.
data DockerContainer = DockerContainer
  { _dockerContainerId :: !T.Text
  , _dockerContainerName :: !T.Text
  , _dockerContainerImage :: !T.Text
  , _dockerContainerCreated :: !Time.UTCTime
  , _dockerContainerRunning :: !Bool
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DockerContainer)


instance ADC.HasCodec DockerContainer where
  codec =
    _codec ADC.<?> "Docker Container Information"
    where
      _codec =
        ADC.object "DockerContainer" $
          DockerContainer
            <$> ADC.requiredField "id" "ID of the container.." ADC..= _dockerContainerId
            <*> ADC.requiredField "name" "Name of the container." ADC..= _dockerContainerName
            <*> ADC.requiredField "image" "Image the container is created from." ADC..= _dockerContainerImage
            <*> ADC.requiredField "created" "Date/time when the container is created at." ADC..= _dockerContainerCreated
            <*> ADC.requiredField "running" "Indicates if the container is running." ADC..= _dockerContainerRunning


-- * SSH Public Key Information


-- | Data definition for SSH public key information.
data SshPublicKey = SshPublicKey
  { _sshPublicKeyData :: !T.Text
  , _sshPublicKeyType :: !T.Text
  , _sshPublicKeyLength :: !Int32
  , _sshPublicKeyComment :: !T.Text
  , _sshPublicKeyFingerprint :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec SshPublicKey)


instance ADC.HasCodec SshPublicKey where
  codec =
    _codec ADC.<?> "SSH Public Key Information"
    where
      _codec =
        ADC.object "SshPublicKey" $
          SshPublicKey
            <$> ADC.requiredField "data" "Original information." ADC..= _sshPublicKeyData
            <*> ADC.requiredField "type" "Type of the public key." ADC..= _sshPublicKeyType
            <*> ADC.requiredField "length" "Length of the public key." ADC..= _sshPublicKeyLength
            <*> ADC.requiredField "comment" "Comment on the public key." ADC..= _sshPublicKeyComment
            <*> ADC.requiredField "fingerprint" "Fingerprint of the public key." ADC..= _sshPublicKeyFingerprint

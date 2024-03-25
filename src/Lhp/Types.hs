{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines public data and type definitions to represent
-- a complete lhp report.
module Lhp.Types where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)


-- * Host


-- | Data definition for host descriptor.
data Host = Host
  { _hostName :: !T.Text
  , _hostUrl :: !(Maybe T.Text)
  , _hostTags :: ![T.Text]
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
            <*> ADC.optionalField "url" "URL to external host information." ADC..= _hostUrl
            <*> ADC.optionalFieldWithDefault "tags" [] "Arbitrary tags for the host." ADC..= _hostTags


-- * Report


-- | Data definition for host patrol report.
data Report = Report
  { _reportHost :: !Host
  , _reportCloud :: !Cloud
  , _reportHardware :: !Hardware
  , _reportKernel :: !Kernel
  , _reportDistribution :: !Distribution
  , _reportDockerContainers :: !(Maybe [DockerContainer])
  , _reportSshAuthorizedKeys :: ![T.Text]
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
            <$> ADC.requiredField "host" "Host descriptor." ADC..= _reportHost
            <*> ADC.requiredField "cloud" "Cloud information." ADC..= _reportCloud
            <*> ADC.requiredField "hardware" "Hardware information." ADC..= _reportHardware
            <*> ADC.requiredField "kernel" "Kernel information." ADC..= _reportKernel
            <*> ADC.requiredField "distribution" "Distribution information." ADC..= _reportDistribution
            <*> ADC.requiredField "dockerContainers" "List of Docker containers if the host is a Docker host." ADC..= _reportDockerContainers
            <*> ADC.requiredField "sshAuthorizedKeys" "List of SSH authorized keys found on host." ADC..= _reportSshAuthorizedKeys


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
  { _hardwareCpuCount :: !Int
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

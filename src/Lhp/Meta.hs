{-# LANGUAGE OverloadedStrings #-}

-- | This module provides project metadata information definitions.
module Lhp.Meta where

import qualified Data.Text as T
import Data.Version (Version, showVersion)
import qualified Paths_lhp as Paths


-- | Application name.
--
-- >>> name
-- "lhp"
name :: T.Text
name = "lhp"


-- | Application title.
--
-- >>> title
-- "Lazy Hacker's Linux Host Patrol"
title :: T.Text
title = "Lazy Hacker's Linux Host Patrol"


-- | Application version.
--
-- > version
-- Version {versionBranch = [0,0,0], versionTags = []}
version :: Version
version = Paths.version


-- | Application version as a 'String' value.
--
-- > versionString
-- "0.0.0"
versionString :: String
versionString = showVersion version


-- | Application version as a 'T.Text' value.
--
-- > versionText
-- "0.0.0"
versionText :: T.Text
versionText = T.pack versionString

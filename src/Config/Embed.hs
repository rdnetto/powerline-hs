{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Config.Embed (embedAllFiles) where

import BasicPrelude
import Data.FileEmbed (embedDir)
import Language.Haskell.TH.Syntax (runIO, Q, Exp)
import Prelude ()
import Safe (fromJustNote, lastMay)

import PythonSite


-- Splice for embedding the contents of the config_files directory.
embedAllFiles :: Q Exp
embedAllFiles = embedDir =<< runIO sysConfigDir

-- Returns the config_files directory that is part of the powerline package installation.
-- We take the last element rather than the first to default to the latest version of Python.
sysConfigDir :: IO String
sysConfigDir = (</> "config_files") . fromJustNote "Failed to find powerline installation" . lastMay <$> pySiteDirs "powerline"


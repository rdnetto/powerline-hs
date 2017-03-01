{-# LANGUAGE TemplateHaskell #-}

module Config.PowerlineResources where

import BasicPrelude
import Data.FileEmbed (embedDir)
import Data.Map as Map
import Git.Embed (embedGit)
import Language.Haskell.TH (Q, Exp)


-- Map of paths (relative to config_files) to contents.
configFiles :: Map FilePath ByteString
configFiles = Map.fromList $(embedDir "./powerline/powerline/config_files")

-- Splice for embedding Powerline version
embedPowerlineVersion :: Q Exp
embedPowerlineVersion = embedGit ["-C", "./powerline/powerline/config_files", "describe", "--tags", "--dirty"]


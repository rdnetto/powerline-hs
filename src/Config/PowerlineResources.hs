{-# LANGUAGE TemplateHaskell #-}

module Config.PowerlineResources where

import BasicPrelude
import Data.FileEmbed (embedDir)
import qualified Data.Map as Map
import Git.Embed (embedGit)
import Language.Haskell.TH (Q, Exp(LitE), Lit(StringL), runIO)
import Prelude ()
import System.Environment (lookupEnv)

-- Map of paths (relative to config_files) to contents.
configFiles :: Map FilePath ByteString
configFiles = Map.fromList $(embedDir "./powerline/powerline/config_files")

-- Splice for embedding Powerline version
embedPowerlineVersion :: Q Exp
embedPowerlineVersion = embedGit ["-C", "./powerline/powerline/config_files", "describe", "--tags", "--dirty"]

-- Attempt to read the value from an environment variable, or fallback to the specified expression
getBuildEnvOr :: String -> Q Exp -> Q Exp
getBuildEnvOr name def = do
    value <- map (LitE . StringL) <$> runIO (lookupEnv name)

    case value of
         Just x  -> return x
         Nothing -> def

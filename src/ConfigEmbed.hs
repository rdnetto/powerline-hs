{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ConfigEmbed where

import BasicPrelude
import Data.FileEmbed (embedFile)
import Language.Haskell.TH (litE)
import Language.Haskell.TH.Syntax (runIO, Q, Exp, Lit(StringL))
import Prelude ()
import Safe (fromJustNote, lastMay)
import System.Directory (doesFileExist)

import PythonSite


-- Represents a config file, which may either be a asset in the binary or a file read at runtime
-- The type parameter represents the parsed contents of the file, as produce by readFile.
data ConfigFile a = UserFile FilePath                   -- ^ FilePath is relative to config dir
                  | InlineFile FilePath ByteString      -- ^ FilePath is absolute path at compile-time
                  deriving Show


-- Splice for embedding JSON of the specified type from one of the provided paths.
-- Expands to a value of type 'ConfigFile a'
embedConfig :: IO [FilePath] -> Q Exp
embedConfig fps = do
    fps' <- runIO fps
    let msg = "Failed to find powerline config file in " ++ show fps'
    path <- runIO $ (fromJustNote msg . lastMay) <$> (filterM doesFileExist fps')
    let path' = litE $ StringL path
    let rawExp = embedFile path

    -- Although we could parse this at compile-time and persist the AST instead,
    -- that would be rather unwieldy as it forces us to provide Lift instances for all the types in ConfigSchema
    [| InlineFile $(path') $(rawExp) |]

-- Helper method for common case
embedConfigFile :: FilePath -> Q Exp
embedConfigFile filename = embedConfig $ map f sysConfigDir where
    f = return . (</> filename)

-- Returns the config_files directory that is part of the powerline package installation.
-- We take the last element rather than the first to default to the latest version of Python.
-- This should really be in ConfigFile, but can't be because it's used in a TemplateHaskell splice.
sysConfigDir :: IO String
sysConfigDir = (</> "config_files") . fromJustNote "Failed to find powerline installation" . lastMay <$> pySiteDirs "powerline"


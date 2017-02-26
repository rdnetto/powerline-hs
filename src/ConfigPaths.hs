{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ConfigPaths (CfgM, runCfg, mainConfigPaths, colorsPaths, colorschemePaths, themePaths) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import qualified Data.Map.Lazy as Map
import Data.Maybe (maybeToList)
import Safe
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((</>))

import CommandArgs (RendererArgs)
import ConfigSchema(
    MainConfig(..),
    ExtConfigs(shell),
    ExtConfig(..),
    defaultTopTheme
    )
import PythonSite
import Util


-- Caches result of looking up paths
data ConfigPathContext = ConfigPathContext {
                            cfgDirs :: [FilePath]
                       } deriving Show

-- Monad transformer stack
newtype CfgM a = CfgM (ReaderT ConfigPathContext IO a)
    deriving (MonadReader ConfigPathContext, MonadIO, Monad, Applicative, Functor)

runCfg :: CfgM a -> IO a
runCfg (CfgM m) = do
    cfg <- ConfigPathContext <$> findCfgDirs
    runReaderT m cfg


findCfgDirs :: IO [FilePath]
findCfgDirs = do
    cfgDir     <- getUserConfigDir "powerline"
    rootCfgDir <- map2 (</> "config_files") getSysConfigDir
    return $ maybeToList rootCfgDir ++ [cfgDir]

-- Returns the config_files directory that is part of the powerline package installation.
-- We take the last element rather than the first to default to the latest version of Python.
getSysConfigDir :: IO (Maybe String)
getSysConfigDir = lastMay <$> pySiteDirs "powerline"

-- Paths to config.json
mainConfigPaths :: CfgM [FilePath]
mainConfigPaths = map (</> "config.json") <$> asks cfgDirs

-- Paths to colors.json
colorsPaths :: CfgM [FilePath]
colorsPaths = map (</> "colors.json") <$> asks cfgDirs

-- Paths to color schemes
colorschemePaths :: MainConfig -> CfgM [FilePath]
colorschemePaths config = f <$> asks cfgDirs where
    extConfig = shell . ext $ config
    f cfgDirs' = do     -- list monad
        n <- ["__main__", colorscheme extConfig]
        d <- ["colorschemes", "colorschemes/shell"]
        base <- cfgDirs'
        return $ base </> d </> n ++ ".json"

-- Paths to themes (define list of segments)
themePaths :: MainConfig -> RendererArgs -> CfgM [FilePath]
themePaths config rendererArgs = f <$> asks cfgDirs where
    f cfgDirs' = do     -- list monad
        base <- cfgDirs'
        theme <- [
                default_top_theme ++ ".json",
                "shell" </> "__main__.json",
                "shell" </> shellTheme ++ ".json"
            ]
        return $ base </> "themes" </> theme

    default_top_theme = defaultTopTheme $ common config

    -- Local themes are used for select, continuation modes (PS3, PS4)
    extConfig = shell . ext $ config
    localTheme = Map.lookup "local_theme" rendererArgs
    shellTheme = case localTheme of
                      Just lt -> Map.findWithDefault lt lt (localThemes extConfig)
                      Nothing -> theme extConfig


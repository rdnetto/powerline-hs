{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.List (foldl1')
import Data.Map (union, findWithDefault)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BSL

import ConfigParser
import Aeson_Merge


main :: IO ()
main = do
    let cfgDir = "/home/reuben/.config/powerline/"
    config  <- loadConfigFile $ cfgDir </> "config.json" :: IO MainConfig
    colours <- loadConfigFile $ cfgDir </> "colors.json" :: IO ColourConfig

    let ExtConfig shellCS shellTheme = shell . ext $ config

    -- color scheme
    let csFilename = shellCS ++ ".json"
    c1 <- groups <$> loadConfigFile (cfgDir </> "colorschemes" </> csFilename)
    c2 <- groups <$> loadConfigFile (cfgDir </> "colorschemes/shell" </> csFilename)
    let cs = union c2 c1

    -- themes - more complicated because we need to merge before parsing
    let default_top_theme = string $ findWithDefault (String "powerline") "default_top_theme" (common config)
    let themeNames = [
                    default_top_theme,
                    "shell" </> "__main__.json",
                    "shell" </> shellTheme ++ ".json"
                 ]
    let themePaths = (cfgDir </>) . ("themes" </>) <$> themeNames
    themesThatExist <- filterM doesFileExist themePaths
    themeCfg <- loadLayeredConfigFiles themesThatExist :: IO ThemeConfig

    print $ show themeCfg

-- Loads a config file, throwing an exception if there was an error message
loadConfigFile :: FromJSON a => FilePath -> IO a
loadConfigFile path = do
    raw <- BSL.readFile path
    return $ fromRight $ eitherDecode raw

-- Loads multiple config files, merges them, then parses them. Merge is right-biased; last file in the list has precedence.
loadLayeredConfigFiles :: FromJSON a => [FilePath] -> IO a
loadLayeredConfigFiles paths = do
    objs <- mapM loadConfigFile paths :: IO [Value]
    let res = foldl1' mergeJson objs

    -- Convert to target type
    print $ encode res
    return . fromRight . eitherDecode $ encode res

-- Helper function for extracting result
fromRight :: Either String b -> b
fromRight (Left a)  = error a
fromRight (Right b) = b


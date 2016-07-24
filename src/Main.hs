module Main where

import Control.Monad
import Data.Aeson
import Data.List (foldl1')
import Data.Map (findWithDefault)
import Data.Text (pack)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BSL

import Aeson_Merge
import ConfigParser
import Segments


main :: IO ()
main = do
    let cfgDir = "/home/reuben/.config/powerline/"
    config  <- loadConfigFile $ cfgDir </> "config.json" :: IO MainConfig
    colours <- loadConfigFile $ cfgDir </> "colors.json" :: IO ColourConfig

    let ExtConfig shellCS shellTheme = shell . ext $ config

    -- Color scheme
    let csNames = (\dir -> cfgDir </> dir </> shellCS ++ ".json") <$> [
                    "colorschemes",
                    "colorschemes/shell"
                ]
    cs <- loadLayeredConfigFiles csNames :: IO ColourSchemeConfig

    -- Themes - more complicated because we need to merge before parsing
    let default_top_theme = string $ findWithDefault (String $ pack "powerline") "default_top_theme" (common config)
    let themeNames = [
                    default_top_theme,
                    "shell" </> "__main__.json",
                    "shell" </> shellTheme ++ ".json"
                 ]
    let themePaths = (cfgDir </>) . ("themes" </>) <$> themeNames
    themesThatExist <- filterM doesFileExist themePaths
    themeCfg <- loadLayeredConfigFiles themesThatExist :: IO ThemeConfig

    -- Generate prompt
    {-
     - TODO WIP: Program execution currently takes 4 ms, which is negligible.
     -           For comparison, Python takes ~100 ms per side (200 ms), and a bash script takes ~5 ms for command execution (zsh is ~8 ms).
     -           NOTE: stack exec has *a lot* of overhead - not suitable for timing.
     -}
    let left_prompt = generateSegment <$> left (segments themeCfg)
    let right_prompt = generateSegment <$> right (segments themeCfg)

    putStrLn "Left:"
    putStrLn $ unlines left_prompt

    putStrLn "Right:"
    putStrLn $ unlines right_prompt

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
    return . fromRight . eitherDecode $ encode res

-- Helper function for extracting result
fromRight :: Either String b -> b
fromRight (Left a)  = error a
fromRight (Right b) = b


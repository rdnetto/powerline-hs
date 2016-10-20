-- TODO: split this file up - too much config logic in here
module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.List (foldl1', intercalate)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy.Merge
import Data.Maybe (catMaybes, fromJust, fromMaybe, maybeToList)
import Rainbow (byteStringMakerFromEnvironment)
import Safe (headMay)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((</>), takeExtension)

import Aeson_Merge
import CommandArgs
import ConfigSchema(
    ColourConfig(..),
    ColourSchemeConfig(..),
    ExtConfig(..),
    ExtConfigs(..),
    ForBothSides(..),
    MainConfig(..),
    Segment(..),
    SegmentArgs,
    SegmentData(..),
    ThemeConfig(..),
    defaultTopTheme,
    )
import PythonSite
import Rendering
import Segments
import Util


main :: IO ()
main = parseArgs >>= \args -> do
    cfgDir     <- getUserConfigDir "powerline"
    rootCfgDir <- map2 (</> "config_files") getSysConfigDir
    let cfgDirs = maybeToList rootCfgDir ++ [cfgDir]

    let loadConfigFile' f = loadLayeredConfigFiles $ (</> f) <$> cfgDirs
    config  <- loadConfigFile' "config.json" :: IO MainConfig
    colours <- loadConfigFile' "colors.json" :: IO ColourConfig

    let ExtConfig shellCS shellTheme = shell . ext $ config

    -- Color scheme
    let csNames = do
            d <- ["colorschemes", "colorschemes/shell"]
            base <- cfgDirs
            return $ base </> d </> shellCS ++ ".json"

    rootCS <- loadLayeredConfigFiles csNames :: IO ColourSchemeConfig

    let modeCS = fromMaybe Map.empty $ do
            -- ZSH allows users to define arbitrary modes, so we can't rely on a translation existing for one
            mode <- Map.lookup "mode" (rendererArgs args)
            cs   <- Map.lookup  mode  (modeTranslations rootCS)
            return $ groups cs

    let rightBiasedMerge = merge preserveMissing preserveMissing $ zipWithMatched (\_ _ r -> r)
    let cs = rightBiasedMerge (groups rootCS) modeCS

    -- Themes - more complicated because we need to merge before parsing
    let default_top_theme = defaultTopTheme $ common config
    let themePaths = do
            cfg <- cfgDirs
            theme <- [
                    default_top_theme ++ ".json",
                    "shell" </> "__main__.json",
                    "shell" </> shellTheme ++ ".json"
                ]
            return $ cfg </> "themes" </> theme

    themesThatExist <- filterM doesFileExist themePaths
    themeCfg <- loadLayeredConfigFiles themesThatExist :: IO ThemeConfig

    -- Needed for rendering
    let numSpaces = fromMaybe 1 $ spaces themeCfg
    let divCfg = themeCfg & dividers & fromJust
    let renderInfo = RenderInfo colours cs divCfg numSpaces
    putChunks' <- putChunks (rendererModule args) <$> byteStringMakerFromEnvironment

    case debugSegment args of
        Just segName -> do
            segs <- generateSegment args . layerSegments (segment_data themeCfg) $ Segment segName Nothing Nothing Nothing
            putChunks' $ renderSegments renderInfo SLeft segs
            putStrLn ""

        Nothing -> do
            -- Need to segments over segment_data
            let fmap2 f = fmap (fmap f)
            let segments' = layerSegments (segment_data themeCfg) `fmap2` segments themeCfg

            -- Generate prompt
            when (renderSide args `elem` [RSLeft, RSAboveLeft]) $ do
                left_prompt  <- generateSegment args `mapM` left  segments'
                putChunks' . renderSegments renderInfo SLeft $ concat left_prompt

            when (renderSide args == RSRight) $ do
                right_prompt <- generateSegment args `mapM` right segments'
                putChunks' . renderSegments renderInfo SRight $ concat right_prompt



-- Returns the config_files directory that is part of the powerline package installation.
getSysConfigDir :: IO (Maybe String)
getSysConfigDir = headMay <$> pySiteDirs "powerline"

-- Loads a config file, throwing an exception if there was an error message
loadConfigFile :: FromJSON a => FilePath -> IO a
loadConfigFile path = do
    raw <- BSL.readFile path
    return $ fromRight . mapLeft (++ "when parsing\n" ++ path) $ eitherDecode raw

-- Loads multiple config files, merges them, then parses them. Merge is right-biased; last file in the list has precedence.
loadLayeredConfigFiles :: FromJSON a => [FilePath] -> IO a
loadLayeredConfigFiles paths = do
    objs <- mapM loadConfigFile paths :: IO [Value]
    let res = foldl1' mergeJson objs

    -- Convert to target type
    return . fromRight . mapLeft (++ " when parsing\n" ++ intercalate "\n" paths) . eitherDecode $ encode res

-- Layer a segment over the corresponding segment data
layerSegments :: Map.Map String SegmentData -> Segment -> Segment
layerSegments segmentData s = Segment (function s) before' after' args' where
    -- segments can be referred to by either their fully qualified name, or its last component
    -- for e.g. powerline.segments.common.net.hostname, it's 'hostname'
    key = function s
    abbreviatedKey = tail $ takeExtension key

    sd = headMay $ catMaybes [
            Map.lookup key segmentData,
            Map.lookup abbreviatedKey segmentData
        ]
    before' = before s `orElse` (sdBefore =<< sd)
    after'  = after s  `orElse` (sdAfter =<< sd)
    args' = Just $ leftBiasedMerge (maybeMap $ args s) (maybeMap $ sdArgs =<< sd)

    leftBiasedMerge :: SegmentArgs -> SegmentArgs -> SegmentArgs
    leftBiasedMerge = merge preserveMissing preserveMissing $ zipWithMatched (\_ -> flip mergeJson)
    maybeMap = fromMaybe Map.empty

-- Helper function for extracting result
fromRight :: Either String b -> b
fromRight (Left a)  = error a
fromRight (Right b) = b


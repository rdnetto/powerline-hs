-- TODO: split this file up - too much config logic in here
module Main where

import Control.Exception (Handler(..), SomeException, catches)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.List (foldl1', intercalate)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy.Merge
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.LocalTime (getZonedTime)
import Rainbow (byteStringMakerFromEnvironment)
import Safe
import System.Directory (doesFileExist, getHomeDirectory)
import System.Exit (exitFailure, ExitCode)
import System.FilePath ((</>), takeExtension)

import Aeson_Merge
import CommandArgs
import ConfigPaths
import ConfigSchema(
    ColourConfig(..),
    ColourSchemeConfig(..),
    ForBothSides(..),
    MainConfig(..),
    Segment(..),
    SegmentArgs,
    SegmentData(..),
    ThemeConfig(..)
    )
import Rendering
import Segments
import Util


main :: IO ()
main = handleErrors $ parseArgs >>= \args -> runCfg $ do
    config  <- loadLayeredConfigFiles mainConfigPaths           :: CfgM MainConfig
    colours <- loadLayeredConfigFiles colorsPaths               :: CfgM ColourConfig
    rootCS  <- loadLayeredConfigFiles $ colorschemePaths config :: CfgM ColourSchemeConfig

    let modeCS = fromMaybe Map.empty $ do
            -- ZSH allows users to define arbitrary modes, so we can't rely on a translation existing for one
            mode <- Map.lookup "mode" (rendererArgs args)
            cs   <- Map.lookup  mode  (modeTranslations rootCS)
            return $ groups cs

    let rightBiasedMerge = merge preserveMissing preserveMissing $ zipWithMatched (\_ _ r -> r)
    let cs = rightBiasedMerge (groups rootCS) modeCS

    -- Themes - more complicated because we need to merge before parsing
    -- see https://powerline.readthedocs.io/en/master/configuration/reference.html#extension-specific-configuration
    themeCfg <- loadLayeredConfigFiles $ themePaths config $ rendererArgs args :: CfgM ThemeConfig

    -- Needed for rendering.
    let numSpaces = fromMaybe 1 $ spaces themeCfg
    let divCfg = themeCfg & dividers & fromJustNote "Could not find dividers info"
    let renderInfo = RenderInfo colours cs divCfg numSpaces
    putChunks' <- liftIO $ putChunks (rendererModule args) <$> byteStringMakerFromEnvironment

    liftIO $ case debugSegment args of
        Just segName -> do
            segs <- generateSegment args . layerSegments (segment_data themeCfg) $ Segment segName Nothing Nothing Nothing
            putChunks' $ renderSegments renderInfo SLeft segs
            putStrLn ""

        Nothing -> do
            -- Need to segments over segment_data
            -- This seems to be specified by config_files/config.json, in the local themes section. Note that we already parse this file for other info
            let segments' = layerSegments (segment_data themeCfg) `map2` segments themeCfg

            let (side, segs) = case renderSide args of
                                    RSLeft      -> (SLeft,  left  segments')
                                    RSAboveLeft -> (SLeft,  left  segments')
                                    RSRight     -> (SRight, right segments')

            prompt <- generateSegment args `mapM` segs
            putChunks' . renderSegments renderInfo side $ concat prompt


-- Loads a config file, throwing an exception if there was an error message
loadConfigFile :: FromJSON a => FilePath -> IO a
loadConfigFile path = do
    raw <- BSL.readFile path
    return $ fromRight . mapLeft (++ "when parsing\n" ++ path) $ eitherDecode raw

-- Loads multiple config files, merges them, then parses them. Merge is right-biased; last file in the list has precedence.
loadLayeredConfigFiles :: FromJSON a => CfgM [FilePath] -> CfgM a
loadLayeredConfigFiles pathGetter = do
    paths <- pathGetter
    paths' <- liftIO $ filterM doesFileExist paths
    when (null paths') $ error ("No existing file found in: " ++ show paths)

    objs <- liftIO $ mapM loadConfigFile paths' :: CfgM [Value]
    let res = foldl1' mergeJson objs

    -- Convert to target type
    return . fromRight . mapLeft (++ " when parsing\n" ++ intercalate "\n" paths') . eitherDecode $ encode res

-- Layer a segment over the corresponding segment data
layerSegments :: Map.Map String SegmentData -> Segment -> Segment
layerSegments segmentData s = Segment (function s) before' after' args' where
    -- segments can be referred to by either their fully qualified name, or its last component
    -- for e.g. powerline.segments.common.net.hostname, it's 'hostname'
    key = function s
    abbreviatedKey = tailMay $ takeExtension key    -- takeExtension returns ".hostname" or "" if no extension found

    sd = headMay $ catMaybes [
            Map.lookup key segmentData,
            join $ Map.lookup <$> abbreviatedKey <*> pure segmentData
        ]
    before' = before s `orElse` (sdBefore =<< sd)
    after'  = after s  `orElse` (sdAfter =<< sd)
    args' = Just $ leftBiasedMerge (maybeMap $ args s) (maybeMap $ sdArgs =<< sd)

    leftBiasedMerge :: SegmentArgs -> SegmentArgs -> SegmentArgs
    leftBiasedMerge = merge preserveMissing preserveMissing $ zipWithMatched (\_ -> flip mergeJson)
    maybeMap = fromMaybe Map.empty

-- Catches exceptions and logs them before terminating
handleErrors :: IO () -> IO ()
handleErrors io = io `catches` [
        -- Don't log cases where we intentionally terminate. e.g. --help
        Handler ( \(_ :: ExitCode) -> return () ),
        Handler logErrors
    ] where

    logErrors :: SomeException -> IO ()
    logErrors e = do
        now <- getZonedTime
        home <- getHomeDirectory
        let txt = show now ++ " " ++  show e ++ "\n\n"

        print e
        appendFile (home </> "powerline-hs.log") txt
        exitFailure


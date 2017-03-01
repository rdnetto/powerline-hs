{-# LANGUAGE TemplateHaskell #-}

module Config.File where

import BasicPrelude
import Data.Aeson (FromJSON, Value, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map as Map
import Data.Maybe (catMaybes)
import Prelude ()
import Safe (fromJustNote)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigFile)

import Aeson.Merge
import CommandArgs (RendererArgs)
import Config.PowerlineResources (configFiles)
import Config.Schema
import Util


-- Represents a config file, which may either be a asset in the binary or a file read at runtime
-- The type parameter represents the parsed contents of the file, as produce by readFile.
-- Paths are relative to config_files .
data ConfigFile a = UserFile FilePath
                  | InlineFile FilePath ByteString
                  deriving Show


-- Returns a JSON Value from confg file. Throws exception on parsing error, returns Nothing if file does not exist.
readConfigFile :: ConfigFile a -> IO (Maybe Value)
readConfigFile cf@(InlineFile _ bs) = return . Just $ readConfigFile' (show cf) (BSL.fromStrict bs)
readConfigFile cf@(UserFile fp) = do
    path <- getUserConfigFile "powerline" fp
    exists <- doesFileExist path
    case exists of
         True  -> Just . readConfigFile' (show cf) <$> BSL.readFile path
         False -> return Nothing

readConfigFile' :: String -> LByteString -> Value
readConfigFile' cf raw = fromRight . mapLeft (++ "when parsing JSON from\n" ++ cf) $ eitherDecode raw

-- Combines multiple ConfigFiles into a single JSON file and parses it.
-- Non-existent files will be ignored.
readLayeredConfigFiles :: FromJSON a => [ConfigFile a] -> IO a
readLayeredConfigFiles cfgs = do
    objs <- mapM readConfigFile cfgs
    let res = foldl1' mergeJson $ catMaybes objs

    -- Convert to target type
    case eitherDecode (encode res) of
         Left msg -> error $ msg ++ " when parsing merged JSON from:\n" ++ intercalate "\n" (show <$> cfgs)
         Right v  -> return v

lookupEmbeddedFile :: FilePath -> Maybe (ConfigFile a)
lookupEmbeddedFile fp = InlineFile fp <$> Map.lookup fp configFiles

lookupEmbeddedFileX :: FilePath -> ConfigFile a
lookupEmbeddedFileX fp = fromJustNote m $ lookupEmbeddedFile fp where
    m = "Could not find embedded file: " ++ fp


mainConfigFiles :: [ConfigFile MainConfig]
mainConfigFiles = [lookupEmbeddedFileX "config.json", UserFile "config.json"]

colourConfigFiles :: [ConfigFile ColourConfig]
colourConfigFiles = [lookupEmbeddedFileX "colors.json", UserFile "colors.json"]

colourSchemeConfigFiles :: MainConfig -> [ConfigFile ColourSchemeConfig]
colourSchemeConfigFiles config = res where
    cs = colorscheme . shell . ext $ config
    res = catMaybes $ do
        n <- ["__main__", cs]
        d <- ["colorschemes", "colorschemes/shell"]
        base <- [lookupEmbeddedFile, Just . UserFile]
        return . base $ d </> n ++ ".json"

themeConfigFiles :: MainConfig -> RendererArgs -> [ConfigFile ThemeConfig]
themeConfigFiles config rendererArgs = res where
    default_top_theme = defaultTopTheme $ common config
    extConfig = shell . ext $ config

    -- Local themes are used for select, continuation modes (PS3, PS4)
    localTheme = Map.lookup "local_theme" rendererArgs
    shellTheme = case localTheme of
                      Just lt -> Map.findWithDefault lt lt (localThemes extConfig)
                      Nothing -> theme extConfig

    res = catMaybes $ do
        base  <- [lookupEmbeddedFile, Just . UserFile]
        theme <- [
            default_top_theme ++ ".json",
            "shell" </> "__main__.json",
            "shell" </> shellTheme ++ ".json"
            ]
        return . base $ "themes" </> theme


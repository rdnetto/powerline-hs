{-# LANGUAGE TemplateHaskell #-}

module ConfigFile (
    module ConfigFile,
    ConfigFile(..)
    ) where

import BasicPrelude
import Data.Aeson (FromJSON, Value, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Prelude ()
import System.Environment.XDG.BaseDir (getUserConfigFile)

import Aeson_Merge
import ConfigEmbed
import ConfigSchema
import Util


-- Returns a JSON Value from confg file. Throws exception on parsing error.
readConfigFile :: ConfigFile a -> IO Value
readConfigFile cf@(InlineFile _ bs) = return $ readConfigFile' (show cf) (BSL.fromStrict bs)
readConfigFile cf@(UserFile fp) = do
    path <- getUserConfigFile "powerline" fp
    raw  <- BSL.readFile path
    return $ readConfigFile' (show cf) raw

readConfigFile' :: String -> LByteString -> Value
readConfigFile' cf raw = fromRight . mapLeft (++ "when parsing JSON from\n" ++ cf) $ eitherDecode raw

-- Combines multiple ConfigFiles into a single JSON file and parses it.
readLayeredConfigFiles :: FromJSON a => [ConfigFile a] -> IO a
readLayeredConfigFiles cfgs = do
    objs <- mapM readConfigFile cfgs
    let res = foldl1' mergeJson objs

    -- Convert to target type
    case eitherDecode (encode res) of
         Left msg -> error $ msg ++ " when parsing merged JSON from:\n" ++ intercalate "\n" (show <$> cfgs)
         Right v  -> return v


mainConfigFiles :: [ConfigFile MainConfig]
mainConfigFiles = [$(embedConfigFile "config.json"), UserFile "config.json"]

colourConfigFiles :: [ConfigFile ColourConfig]
colourConfigFiles = [$(embedConfigFile "colors.json"), UserFile "colors.json"]


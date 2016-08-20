{-# LANGUAGE RecordWildCards #-}

module Segments(generateSegment) where

import Data.Maybe (fromMaybe)

import Data.Map.Lazy as Map
import Data.Time.LocalTime (getZonedTime)
import Network.BSD as Net
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import ConfigSchema (Segment(..))
import Util


-- TODO: figure out how segments can log failure
-- TODO: actually the segment args

-- Map of segments to their handlers
segmentHandlers :: Map.Map String (IO (Maybe String))
segmentHandlers = fromList [
        ("powerline.segments.common.env.user", lookupEnv "USER"),
        ("powerline.segments.common.env.virtualenv", lookupEnv "VIRTUAL_ENV"),
        ("powerline.segments.common.net.hostname",  Just <$> Net.getHostName),
        ("powerline.segments.common.time.date", Just . show <$> getZonedTime),
        ("powerline.segments.common.vcs.branch", gitBranch),
        ("powerline.segments.shell.cwd", Just <$> getCurrentDirectory),
        ("powerline.segments.shell.jobnum", lookupEnv "_POWERLINE_JOBNUM")
    ]

-- Execute a segment
generateSegment :: Segment -> IO String
generateSegment Segment {..} = do
    let fM = fromMaybe ""
    body <- Map.findWithDefault (return . Just $ red "???") function segmentHandlers
    return $ case body of
                Nothing -> ""
                Just body' -> function ++ ": " ++ fM before ++ body' ++ fM after

-- TODO: add support for other VCSs
gitBranch :: IO (Maybe String)
gitBranch = do
    abbreviated <- readProcess "git" ["rev-parse", "--short", "--abbrev-ref", "HEAD"]

    case abbreviated of
        -- detached branch
        Just "HEAD" -> readProcess "git" ["rev-parse", "--short", "HEAD"]
        x           -> return x

-- Helper function for error handling
red :: String -> String
red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"


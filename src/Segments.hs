{-# LANGUAGE RecordWildCards #-}

module Segments(generateSegment) where

import Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime (getZonedTime)
import Network.BSD as Net
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import ConfigSchema (Segment(..), SegmentArgs)
import Util


-- TODO: figure out how segments can log failure

-- Placeholder for info passed in via command-line arguments
type PromptContext = ()

type SegmentHandler = SegmentArgs -> PromptContext -> IO (Maybe String)


-- Map of segments to their handlers
segmentHandlers :: Map.Map String SegmentHandler
segmentHandlers = fromList [
        ("powerline.segments.common.env.user",       simpleHandler $ lookupEnv "USER"),
        ("powerline.segments.common.env.virtualenv", simpleHandler $ lookupEnv "VIRTUAL_ENV"),
        ("powerline.segments.common.net.hostname",   simpleHandler $ Just <$> Net.getHostName),
        ("powerline.segments.common.time.date",      simpleHandler $ Just . show <$> getZonedTime),
        ("powerline.segments.common.vcs.branch",     simpleHandler $ gitBranch),
        ("powerline.segments.common.vcs.stash",      simpleHandler $ gitStashCount),
        ("powerline.segments.shell.cwd",             simpleHandler $ Just <$> getCurrentDirectory),
        ("powerline.segments.shell.jobnum",          simpleHandler $ lookupEnv "_POWERLINE_JOBNUM")
    ]

-- Execute a segment
generateSegment :: PromptContext -> Segment -> IO String
generateSegment ctx Segment {..} = do
    let fM = fromMaybe ""
    let handler = Map.findWithDefault missingHandler function segmentHandlers
    body <- handler (fromMaybe Map.empty args) ctx
    return $ case body of
                Nothing -> ""
                Just body' -> function ++ ": " ++ fM before ++ body' ++ fM after

-- Default handler
missingHandler _ _ = return . Just . red $ "???"

-- Wrapper for handlers which don't use any context
simpleHandler :: (IO (Maybe String)) -> SegmentHandler
simpleHandler f = \_ _ -> f

-- TODO: add support for other VCSs
gitBranch :: IO (Maybe String)
gitBranch = do
    abbreviated <- readProcess "git" ["rev-parse", "--short", "--abbrev-ref", "HEAD"]

    case abbreviated of
        -- detached branch
        Just "HEAD" -> readProcess "git" ["rev-parse", "--short", "HEAD"]
        x           -> return x

gitStashCount :: IO (Maybe String)
gitStashCount = ((=<<) $ showNZ . length . lines) <$> readProcess "git" ["stash", "list"] where
    showNZ 0 = Nothing
    showNZ x = Just $ show x

-- Helper function for error handling
red :: String -> String
red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"


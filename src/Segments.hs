{-# LANGUAGE RecordWildCards #-}

module Segments(generateSegment) where

import Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Network.BSD as Net
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import CommandArgs
import ConfigSchema (Segment(..), SegmentArgs, argLookup)
import Segments.Base
import qualified Segments.Common as Common
import qualified Segments.VCS as VCS


-- Map of segments to their handlers
segmentHandlers :: Map.Map String SegmentHandler
segmentHandlers = fromList [
        ("powerline.segments.common.env.user",        simpleHandler $ lookupEnv "USER"),
        ("powerline.segments.common.env.virtualenv",  simpleHandler $ lookupEnv "VIRTUAL_ENV"),
        ("powerline.segments.common.net.hostname",    simpleHandler $ Just <$> Net.getHostName),
        ("powerline.segments.common.time.date",       Common.timeDateSegment),
        ("powerline.segments.common.vcs.branch",      VCS.branchSegment),
        ("powerline.segments.common.vcs.stash",       VCS.stashCountSegment),
        ("powerline.segments.shell.cwd",              simpleHandler $ Just <$> getCurrentDirectory),
        ("powerline.segments.shell.jobnum",           simpleHandler $ lookupEnv "_POWERLINE_JOBNUM"),
        ("powerline.segments.shell.last_pipe_status", contextHandler lastPipeStatus)
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
missingHandler :: SegmentHandler
missingHandler _ _ = return . Just . red $ "???"

-- Helper function for error handling
red :: String -> String
red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"


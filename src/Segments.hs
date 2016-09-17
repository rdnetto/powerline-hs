module Segments(generateSegment) where

import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import qualified Network.BSD as Net
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)

import qualified ConfigSchema as CS
import Segments.Base

import qualified Segments.Common.Net
import qualified Segments.Common.Sys
import qualified Segments.Common.Time
import qualified Segments.Shell
import qualified Segments.VCS


-- Map of segments to their handlers
segmentHandlers :: Map.Map String SegmentHandler
segmentHandlers = Map.fromList [
        ("powerline.segments.common.env.user",              simpleHandler "user" $ lookupEnv "USER"),
        ("powerline.segments.common.env.virtualenv",        simpleHandler "virtualenv" . fmap (fmap takeFileName) $ lookupEnv "VIRTUAL_ENV"),
        ("powerline.segments.common.net.hostname",          simpleHandler "hostname" $ Just <$> Net.getHostName),
        ("powerline.segments.common.sys.cpu_load_percent",  Segments.Common.Sys.cpuLoadPercentSegment),
        ("powerline.segments.common.sys.system_load",       Segments.Common.Sys.cpuLoadAverageSegment),
        ("powerline.segments.common.sys.uptime",            Segments.Common.Sys.uptimeSegment),
        ("powerline.segments.common.time.date",             Segments.Common.Time.timeDateSegment),
        ("powerline.segments.common.vcs.branch",            Segments.VCS.branchSegment),
        ("powerline.segments.common.vcs.stash",             Segments.VCS.stashCountSegment),
        ("powerline.segments.shell.cwd",                    Segments.Shell.cwdSegment),
        ("powerline.segments.shell.jobnum",                 simpleHandler "jobnum" $ lookupEnv "_POWERLINE_JOBNUM"),
        ("powerline.segments.shell.last_pipe_status",       Segments.Shell.pipeStatusSegment)
    ]

-- Execute a segment
generateSegment :: PromptContext -> CS.Segment -> IO [Segment]
generateSegment ctx (CS.Segment sFunc sBefore sAfter sArgs) = do
    let handler = case Map.lookup sFunc segmentHandlers of
                       Just x  -> x
                       Nothing -> missingHandler sFunc

    body <- handler (fromMaybe Map.empty sArgs) ctx

    let concatMaybes = concatMap $ fromMaybe ""
    return $ modifySegText (\body' -> concatMaybes [sBefore, Just body', sAfter]) <$> body

-- Default handler
missingHandler :: String -> SegmentHandler
missingHandler func _ _ = return2 . Segment "background" $ red func

-- Helper function for error handling
red :: String -> String
red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"


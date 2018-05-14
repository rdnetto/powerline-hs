module Segments(generateSegment) where

import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)

import qualified Config.Schema as CS
import Segments.Base

import qualified Segments.Common.Batt
import qualified Segments.Common.Env
import qualified Segments.Common.Net
import qualified Segments.Common.Sys
import qualified Segments.Common.Time
import qualified Segments.Shell
import qualified Segments.VCS


-- Map of segments to their handlers
segmentHandlers :: Map.Map String SegmentHandler
segmentHandlers = Map.fromList [
        ("powerline.segments.common.bat.battery",           Segments.Common.Batt.battSegment),
        ("powerline.segments.common.env.environment",       Segments.Common.Env.envSegment),
        ("powerline.segments.common.env.user",              Segments.Common.Env.userSegment),
        ("powerline.segments.common.env.virtualenv",        simpleHandler "virtualenv" . fmap (fmap takeFileName) $ lookupEnv "VIRTUAL_ENV"),
        ("powerline.segments.common.net.external_ip",       Segments.Common.Net.externalIpSegment),
        ("powerline.segments.common.net.hostname",          Segments.Common.Net.hostnameSegment),
        ("powerline.segments.common.net.internal_ip",       Segments.Common.Net.internalIpSegment),
        ("powerline.segments.common.sys.cpu_load_percent",  Segments.Common.Sys.cpuLoadPercentSegment),
        ("powerline.segments.common.sys.system_load",       Segments.Common.Sys.cpuLoadAverageSegment),
        ("powerline.segments.common.sys.uptime",            Segments.Common.Sys.uptimeSegment),
        ("powerline.segments.common.time.date",             Segments.Common.Time.timeDateSegment),
        ("powerline.segments.common.vcs.branch",            Segments.VCS.branchSegment),
        ("powerline.segments.common.vcs.stash",             Segments.VCS.stashCountSegment),
        ("powerline.segments.shell.continuation",           Segments.Shell.continuationSegment),
        ("continuation",                                    Segments.Shell.continuationSegment),    -- Needed because the default file uses an unqualified name
        ("powerline.segments.shell.cwd",                    Segments.Shell.cwdSegment),
        ("powerline.segments.shell.jobnum",                 Segments.Shell.jobNumSegment),
        ("powerline.segments.shell.last_pipe_status",       Segments.Shell.pipeStatusSegment),
        ("powerline.segments.shell.last_status",            Segments.Shell.lastStatusSegment),
        ("powerline.segments.shell.mode",                   Segments.Shell.modeSegment),
         -- gitstatus segments have a different name format because the original powerline doesn't support git status, using format from https://github.com/jaspernbrouwer/powerline-gitstatus
        ("powerline_gitstatus.gitstatus_ahead",             Segments.VCS.gitCommitsAheadCountSegment),
        ("powerline_gitstatus.gitstatus_behind",            Segments.VCS.gitCommitsBehindCountSegment),
        ("powerline_gitstatus.gitstatus_staged",            Segments.VCS.gitStagedFilesCountSegment),
        ("powerline_gitstatus.gitstatus_unmerged",          Segments.VCS.gitUnmergedFilesCountSegment)
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
missingHandler func _ _ = return2 . Segment hlGroup $ red func where
    hlGroup = HighlightGroup "background" Nothing

-- Helper function for error handling
red :: String -> String
red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"


module Segments(generateSegment) where

import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import qualified Network.BSD as Net
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)

import qualified ConfigSchema as CS
import Segments.Base
import qualified Segments.Common as Common
import qualified Segments.Shell as Shell
import qualified Segments.VCS as VCS


-- Map of segments to their handlers
segmentHandlers :: Map.Map String SegmentHandler
segmentHandlers = Map.fromList [
        ("powerline.segments.common.env.user",        simpleHandler "user" $ lookupEnv "USER"),
        ("powerline.segments.common.env.virtualenv",  simpleHandler "virtualenv" . fmap (fmap takeFileName) $ lookupEnv "VIRTUAL_ENV"),
        ("powerline.segments.common.net.hostname",    simpleHandler "hostname" $ Just <$> Net.getHostName),
        ("powerline.segments.common.sys.uptime",      Common.uptimeSegment),
        ("powerline.segments.common.time.date",       Common.timeDateSegment),
        ("powerline.segments.common.vcs.branch",      VCS.branchSegment),
        ("powerline.segments.common.vcs.stash",       VCS.stashCountSegment),
        ("powerline.segments.shell.cwd",              Shell.cwdSegment),
        ("powerline.segments.shell.jobnum",           simpleHandler "jobnum" $ lookupEnv "_POWERLINE_JOBNUM"),
        ("powerline.segments.shell.last_pipe_status", Shell.pipeStatusSegment)
    ]

-- Execute a segment
generateSegment :: PromptContext -> CS.Segment -> IO [Segment]
generateSegment ctx (CS.Segment sFunc sBefore sAfter sArgs)  = do
    let handler = Map.findWithDefault missingHandler sFunc segmentHandlers
    body <- handler (fromMaybe Map.empty sArgs) ctx

    let concatMaybes = concatMap $ fromMaybe ""

    return $ modifySegText (\body' -> concatMaybes [sBefore, Just body', sAfter]) <$> body

-- Default handler
missingHandler :: SegmentHandler
missingHandler _ _ = return . return $ Segment "" $ red "???"

-- Helper function for error handling
red :: String -> String
red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"


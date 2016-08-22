module Segments(generateSegment) where

import Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Network.BSD as Net
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

import qualified ConfigSchema as CS
import Segments.Base
import qualified Segments.Common as Common
import qualified Segments.Shell as Shell
import qualified Segments.VCS as VCS


-- Map of segments to their handlers
segmentHandlers :: Map.Map String SegmentHandler
segmentHandlers = fromList [
        ("powerline.segments.common.env.user",        simpleHandler "user" $ lookupEnv "USER"),
        ("powerline.segments.common.env.virtualenv",  simpleHandler "virtualenv" $ lookupEnv "VIRTUAL_ENV"),
        ("powerline.segments.common.net.hostname",    simpleHandler "hostname" $ Just <$> Net.getHostName),
        ("powerline.segments.common.time.date",       Common.timeDateSegment),
        ("powerline.segments.common.vcs.branch",      VCS.branchSegment),
        ("powerline.segments.common.vcs.stash",       VCS.stashCountSegment),
        ("powerline.segments.shell.cwd",              simpleHandler "cwd" $ Just <$> getCurrentDirectory),
        ("powerline.segments.shell.jobnum",           simpleHandler "jobnum" $ lookupEnv "_POWERLINE_JOBNUM"),
        ("powerline.segments.shell.last_pipe_status", Shell.pipeStatusSegment)
    ]

-- Execute a segment
generateSegment :: PromptContext -> CS.Segment -> IO [String]
generateSegment ctx (CS.Segment sFunc sBefore sAfter sArgs)  = do
    let fM = fromMaybe ""
    let handler = Map.findWithDefault missingHandler sFunc segmentHandlers
    body <- handler (fromMaybe Map.empty sArgs) ctx
    return $ (\body' -> sFunc ++ ": " ++ fM sBefore ++ body' ++ fM sAfter) . renderSegment <$> body

-- Render a segment
-- TODO: implement
renderSegment :: Segment -> String
renderSegment = segmentText

-- Default handler
missingHandler :: SegmentHandler
missingHandler _ _ = return . return $ Segment "???" $ red "???"

-- Helper function for error handling
red :: String -> String
red s = "\ESC[0;31m" ++ s ++ "\ESC[0m"


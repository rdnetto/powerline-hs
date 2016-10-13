module Segments.Base(
    HighlightGroup(..),
    PromptContext,
    Segment(..),
    SegmentArgs,
    SegmentHandler,
    argLookup,
    argLookup',
    contextHandler,
    getPowerlineFile,
    modifySegText,
    simpleHandler,
    return2,
    ) where

import Control.Monad (liftM)
import Data.Maybe (maybeToList)
import Rainbow (Radiant)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Posix.User (getLoginName)

import CommandArgs
import ConfigSchema (SegmentArgs, argLookup, argLookup')
import Util


-- TODO: figure out how segments can log failure

-- Placeholder for info passed in via command-line arguments
type PromptContext = CommandArgs

type SegmentHandler = SegmentArgs -> PromptContext -> IO [Segment]

-- A rendered segment.
data Segment = Segment {
                    segmentGroup :: HighlightGroup,
                    segmentText  :: String   -- text in the segment
                }
             | Divider {
                    divFore :: Radiant,
                    divBack :: Radiant,
                    divText :: String
                } deriving (Show, Eq)

data HighlightGroup = HighlightGroup {
                        hlGroup :: String,                      -- simple highlight group to use
                        hlGradient :: Maybe (String, Float)     -- gradient and value, if defined
                    } deriving (Show, Eq)

modifySegText :: (String -> String) -> Segment -> Segment
modifySegText f s = s { segmentText = f (segmentText s) }


-- Wrapper for handlers which don't use any context
simpleHandler :: String -> IO (Maybe String) -> SegmentHandler
simpleHandler hlGroupName f _ _ = maybeToList <$> Segment hlGroup' `lift2` f where
    lift2 = liftM . liftM
    hlGroup' = HighlightGroup hlGroupName Nothing

-- Wrapper for handlers which show a value provided in the command args
contextHandler :: Show a => String -> (CommandArgs -> a) -> SegmentHandler
contextHandler hlGroupName field _ args = return2 . Segment hlGroup' . show $ field args where
    hlGroup' = HighlightGroup hlGroupName Nothing

-- Helper method: returns the path to a file with the given name.
-- Ensures the parent directory exists.
-- Used by segments which need to maintain state between invocations.
getPowerlineFile :: String -> IO String
getPowerlineFile name = do
    root <- (</> ".powerline-hs") <$> getTemporaryDirectory
    username <- getLoginName
    createDirectoryIfMissing False root
    return $ root </> name ++ "_" ++ username


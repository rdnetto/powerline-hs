module Segments.Base(
    PromptContext,
    Segment(..),
    SegmentArgs,
    SegmentHandler,
    argLookup,
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
import ConfigSchema (SegmentArgs, argLookup)


-- TODO: figure out how segments can log failure

-- Placeholder for info passed in via command-line arguments
type PromptContext = CommandArgs

type SegmentHandler = SegmentArgs -> PromptContext -> IO [Segment]

-- A rendered segment.
data Segment = Segment {
                    segmentGroup :: String,  -- highlight group used
                    segmentText  :: String   -- text in the segment
                }
             | Divider {
                    divFore :: Radiant,
                    divBack :: Radiant,
                    divText :: String
                } deriving (Show, Eq)

modifySegText :: (String -> String) -> Segment -> Segment
modifySegText f s = s { segmentText = f (segmentText s) }


-- Wrapper for handlers which don't use any context
simpleHandler :: String -> IO (Maybe String) -> SegmentHandler
simpleHandler hlGroup f _ _ = maybeToList <$> Segment hlGroup `lift2` f where
    lift2 = liftM . liftM

-- Wrapper for handlers which show a value provided in the command args
contextHandler :: Show a => String -> (CommandArgs -> a) -> SegmentHandler
contextHandler hlGroup field _ args = return2 . Segment hlGroup . show $ field args

return2 :: (Monad m1, Monad m2) => a -> m1 (m2 a)
return2 = return . return

-- Helper method: returns the path to a file with the given name.
-- Ensures the parent directory exists.
-- Used by segments which need to maintain state between invocations.
getPowerlineFile :: String -> IO String
getPowerlineFile name = do
    root <- (</> ".powerline-hs") <$> getTemporaryDirectory
    username <- getLoginName
    createDirectoryIfMissing False root
    return $ root </> name ++ "_" ++ username


module Segments.Base(
    PromptContext,
    Segment,
    SegmentArgs,
    SegmentHandler,
    argLookup,
    contextHandler,
    simpleHandler,
    ) where

import CommandArgs
import ConfigSchema (Segment(..), SegmentArgs, argLookup)

-- TODO: figure out how segments can log failure

-- Placeholder for info passed in via command-line arguments
type PromptContext = CommandArgs

type SegmentHandler = SegmentArgs -> PromptContext -> IO (Maybe String)


-- Wrapper for handlers which don't use any context
simpleHandler :: IO (Maybe String) -> SegmentHandler
simpleHandler f _ _ = f

-- Wrapper for handlers which show a value provided in the command args
contextHandler :: Show a => (CommandArgs -> a) -> SegmentHandler
contextHandler field _ args = return . Just . show $ field args


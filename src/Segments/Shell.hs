
module Segments.Shell where

import CommandArgs (lastPipeStatus)
import Segments.Base


pipeStatusSegment :: SegmentHandler
pipeStatusSegment _ args = return $ f <$> lastPipeStatus args where
    f 0 = Segment "exit_success" "0"
    f x = Segment "exit_fail" (show x)


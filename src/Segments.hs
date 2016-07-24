module Segments where

import Data.Map

import ConfigParser (Segment(..))


-- segments :: Map String String

generateSegment :: Segment -> String
generateSegment s = function s


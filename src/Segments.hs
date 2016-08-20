{-# LANGUAGE RecordWildCards #-}

module Segments where

import Data.Maybe (fromMaybe)

import Data.Map.Lazy as Map
import Network.BSD as Net

import ConfigSchema (Segment(..))


-- Map of segments to their handlers
segmentHandlers :: Map.Map String (IO String)
segmentHandlers = fromList [
        ("powerline.segments.common.net.hostname", Net.getHostName)
    ]
--
-- Execute a segment
generateSegment :: Segment -> IO String
generateSegment Segment {..} = do
    let fM = fromMaybe ""
    body <- Map.findWithDefault (return "???") function segmentHandlers
    return $ fM before ++ body ++ fM after



module Segments.Common where

import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

import Segments.Base


-- powerline.segments.common.time.date
timeDateSegment :: SegmentHandler
timeDateSegment args _ = do
        let isTime = argLookup args "istime" False
        let fmt = argLookup args "format" "%Y-%m-%d"
        t <- getZonedTime
        return $ Just $ formatTime defaultTimeLocale fmt t


module Segments.Common  where

import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.Clock (diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (getZonedTime)

import Segments.Base


-- powerline.segments.common.time.date
timeDateSegment :: SegmentHandler
timeDateSegment args _ = do
    let isTime = argLookup args "istime" False
    let fmt = argLookup args "format" "%Y-%m-%d"
    let hlGroup = if   isTime
                    then "time"
                    else "date"

    t <- getZonedTime
    return . return $ Segment hlGroup $ formatTime defaultTimeLocale fmt t

-- powerline.segments.common.sys.uptime
-- TODO: implement (has fairly sophisticated formatting rules)
uptimeSegment :: SegmentHandler
uptimeSegment = undefined

-- powerline.segments.common.sys.cpu_load_percent
cpuLoadSegment :: SegmentHandler
cpuLoadSegment args _ = undefined
    -- TODO: ignoring the format arg because supporting it would require us to implement Python-style string formatting

-- How long it has been since the system was booted.
uptime :: IO NominalDiffTime
uptime = do
    let f = posixSecondsToUTCTime
    bt  <- f <$> bootTime
    now <- f <$> getPOSIXTime
    return $ diffUTCTime now bt

-- Time the system was booted at.
bootTime :: IO POSIXTime
bootTime = realToFrac . (read :: String -> Integer)
                      . (!! 1)
                      . head
                      . filter isBtime
                      . map words
                      . lines
                      <$> readFile "/proc/stat"
    where
        isBtime ("btime":_) = True
        isBtime _ = False


{-# LANGUAGE ScopedTypeVariables #-}

module Segments.Common  where

import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.Clock (diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime, posixSecondsToUTCTime)
import Data.Time.LocalTime (getZonedTime)

import Format
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
uptimeSegment :: SegmentHandler
uptimeSegment args _ = do
    let segCount = argLookup args "shorten_len" 3 :: Int
    let daysFmt  :: Int -> String = pyFormat $ argLookup args "days_format"    "{:d}d"
    let hoursFmt :: Int -> String = pyFormat $ argLookup args "hours_format"   "{:d}h"
    let minsFmt  :: Int -> String = pyFormat $ argLookup args "minutes_format" "{:d}m"
    let secsFmt  :: Int -> String = pyFormat $ argLookup args "seconds_format" "{:d}s"

    (days, hrs, mins, secs, _) <- timeComponents <$> uptime
    let comps = [
            (daysFmt, days),
            (hoursFmt, hrs),
            (minsFmt, mins),
            (secsFmt, secs)
            ]
    let comps' = take segCount $ dropWhile ((== 0) . snd) comps
    let tupApply (f, x) = f x
    let txt = unwords $ map tupApply comps'
    return [Segment "background" txt]

timeComponents :: NominalDiffTime -> (Int, Int, Int, Int, Int)
timeComponents t = (days, hrs, mins, secs, ms) where
    (tsecs, mantissa) = properFraction t
    ms = floor $ mantissa * 1000
    (q1, secs)  = divMod tsecs 60
    (q2, mins)  = divMod q1 60
    (days, hrs) = divMod q2 24

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


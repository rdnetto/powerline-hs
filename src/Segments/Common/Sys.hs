module Segments.Common.Sys where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime, posixSecondsToUTCTime)
import GHC.Conc (getNumProcessors)
import Prelude hiding (readFile)
import System.Directory (doesFileExist)
import System.IO.Strict (readFile)

import Format
import Segments.Base
import Util


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
    let hlGroup = HighlightGroup "background" Nothing
    return2 $ Segment hlGroup txt

timeComponents :: NominalDiffTime -> (Int, Int, Int, Int, Int)
timeComponents t = (days, hrs, mins, secs, ms) where
    (tsecs, mantissa) = properFraction t
    ms = floor $ mantissa * 1000
    (q1, secs)  = divMod tsecs 60
    (q2, mins)  = divMod q1 60
    (days, hrs) = divMod q2 24


-- powerline.segments.common.sys.cpu_load_percent
cpuLoadPercentSegment :: SegmentHandler
cpuLoadPercentSegment args _ = do
    -- TODO: gradient support
    let format = pyFormat $ argLookup args "format" "{0:.0f}%"
    let hlGroup = HighlightGroup "cpu_load_percent" Nothing
    usage <- cpuUsage
    return2 . Segment hlGroup $ format usage

-- powerline.segments.common.sys.system_load
cpuLoadAverageSegment :: SegmentHandler
cpuLoadAverageSegment args _ = do
    let format = pyFormat $ argLookup args "format" "{avg:.1f}"
    let thresholdGood = argLookup args "threshold_good" 1 :: Float
    let thresholdBad  = argLookup args "threshold_bad"  2 :: Float

    -- Normalise the load average
    cpuCount <- fromIntegral <$> getNumProcessors
    loadAvgs <- cpuLoadAverage
    let normAvgs = (/cpuCount) <$> loadAvgs


    -- TODO: use thresholds and normalised load to compute gradient value
    let hlGroup = HighlightGroup "system_load" Nothing
    return2 . Segment hlGroup . unwords $ format <$> loadAvgs


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

-- This is the unnormalized load average; the no. of processes in the run queue averaged over 1, 5, 15 min.
cpuLoadAverage :: IO [Float]
cpuLoadAverage = map read . take 3 . words <$> readFile "/proc/loadavg"

-- Returns the CPU usage as a percentage.
--
-- Values in /proc/stat are since boot, so we need to sample twice and take the difference.
-- We use the same approch as psutil, which is to save the current sample in temporary file and use it for subsequent evaluations.
-- This means that the sampling period is the time between invocations, which significantly improves the accuracy.
--
-- See: https://github.com/Leo-G/DevopsWiki/wiki/How-Linux-CPU-Usage-Time-and-Percentage-is-calculated
-- See: proc(5) - /proc/stat
cpuUsage :: IO Float
cpuUsage = do
    -- cpu, user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice
    let readStat = map read . take 8 . drop 1 . words . head . lines <$> readFile "/proc/stat"

    -- To avoid blocking for hundreds of ms while sampling usage, we store the previous values.
    valuesFile <- getPowerlineFile "cpu_usage"

    (vals0, vals1) <- ifM (doesFileExist valuesFile)
                      (do
                            -- Historical data available - compute usage over period since we were last run.
                            vals0 <- map read . words <$> readFile valuesFile
                            vals1 <- readStat

                            writeFile valuesFile . unwords . map show $ vals1
                            return (vals0, vals1)
                      ) (do
                            -- No historical data - need to block until we have enough data
                            vals0 <- readStat

                            -- Keep sampling until we have a value with an accuracy of at least ±10%.
                            vals1 <- untilM $ do
                                threadDelay 10000       -- /proc/stat uses units of CONFIG_HZ, which is typically 10 ms.
                                vals' <- readStat

                                return $ if (sum vals' - sum vals0) < 10
                                         then Nothing
                                         else Just vals'

                            writeFile valuesFile . unwords . map show $ vals1
                            return (vals0, vals1)
                      )

    -- sum [user, nice, system, idle, iowait, irq, softirq, steal]
    let totalTime = sum vals1 - sum vals0

    -- sum [idle, iowait]
    let idle   = (!! 3)
        iowait = (!! 4)
    let idleTime = idle vals1 + iowait vals1 - (idle vals0 + iowait vals0)

    return $ 100 * (1 - idleTime / totalTime)



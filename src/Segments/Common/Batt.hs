{- The original implementation uses the first of the following that works:
 - * dbus+upower
 - * /sys/class/power_supply
 - * pmset (OSX)
 - * Win32 / COM API
 -
 - I have only implemented one for now - others can be added on request.
 -}

module Segments.Common.Batt where

import Control.Monad (filterM)
import Data.List (isPrefixOf)
import Data.Traversable (forM)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>))

import Format
import Segments.Base


data BattStatus = BattStatus {
    charge :: Float,
    charging :: Bool
} deriving (Eq, Show)


-- powerline.segments.common.bat.battery
battSegment :: SegmentHandler
battSegment args _ = do
    let format :: String -> Float -> String = pyFormat $ argLookup args "format" "{ac_state} {capacity:3.0%}"
    let steps      = argLookup args "steps" 5 :: Int
    let gamify     = argLookup args "gamify" False
    let fullHeart  = argLookup args "full_heart" "O"
    let emptyHeart = argLookup args "empty_heart" "O"
    let onlineSym  = argLookup args "online" "C"
    let offlineSym = argLookup args "offline" ""

    return []


-- Retrieve battery status using sysfs (Linux)
sysfsBatt :: IO BattStatus
sysfsBatt = do
    let baseDir = "/sys/class/power_supply"
    let hasEnergy f = doesFileExist $ baseDir </> f </> "energy_now"
    suppliers <- filterM hasEnergy =<< getDirectoryContents baseDir

    let readF fname read' = suppliers `forM` \s -> read' <$> readFile (baseDir </> s </> fname)
    energyNow  <- readF "energy_now" read
    energyFull <- readF "energy_full" read
    charging   <- readF "status" (not . isPrefixOf "Discharging")

    return $ BattStatus (sum energyNow / sum energyFull) (or charging)


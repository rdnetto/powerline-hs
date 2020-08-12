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
import Util


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
    let offlineSym = argLookup args "offline" " "

    let renderSerious BattStatus{..} = Segment hlGroup txt where
            hlGroup = HighlightGroup "battery_gradient" (Just $ 1 - charge)
            txt = format acState (100 * charge)
            acState = if charging
                         then onlineSym
                         else offlineSym

    -- TODO: implement gamelike
    let renderGamelike = renderSerious

    battStat <- takeFirstJust [sysfsBatt]
    return $ case (gamify, battStat) of
                  (True,  Just bs) -> [renderGamelike bs]
                  (False, Just bs) -> [renderSerious bs]
                  (_, Nothing) -> []


-- Retrieve battery status using sysfs (Linux)
sysfsBatt :: IO (Maybe BattStatus)
sysfsBatt = do
    let baseDir = "/sys/class/power_supply"
    let hasEnergy f = doesFileExist $ baseDir </> f </> "charge_now"
    suppliers <- filterM hasEnergy =<< getDirectoryContents baseDir

    let readF fname read' = suppliers `forM` \s -> read' <$> readFile (baseDir </> s </> fname)
    energyNow  <- readF "charge_now" read
    energyFull <- readF "charge_full" read
    charging   <- readF "status" (not . isPrefixOf "Discharging")

    case energyNow of
         [] -> return Nothing
         _  -> return . Just $ BattStatus (sum energyNow / sum energyFull) (or charging)


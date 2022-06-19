module Segments.Common.Net (hostnameSegment, internalIpSegment, externalIpSegment, getGatewayInterface) where

import qualified Data.ByteString.Char8 as ASCII
import Data.Char (isSpace)
import Data.List (sortOn)
import qualified Data.Map.Lazy as Map
import Data.Maybe (isJust, maybeToList)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Network.BSD (getHostName)
import Network.Download (openURI)
import qualified Network.Info as NetInfo
import Prelude hiding (readFile)
import System.Directory (doesFileExist, getModificationTime)
import System.Environment (lookupEnv)
import System.IO.Strict (readFile)

import Segments.Base
import Util


-- powerline.segments.common.net.hostname
hostnameSegment :: SegmentHandler
hostnameSegment args _ = do
    let onlyIfSsh     = argLookup args "only_if_ssh"   False
    let excludeDomain = argLookup args "exclude_domain" False

    enabled <- if onlyIfSsh
               then isJust <$> lookupEnv "SSH_CLIENT"
               else return True

    let dropDomain = takeWhile (/= '.')
    let hlGroup = HighlightGroup "hostname" Nothing

    if enabled
       then return . Segment hlGroup . applyIf excludeDomain dropDomain <$> getHostName
       else return []

-- powerline.segments.common.net.internal_ip
internalIpSegment :: SegmentHandler
internalIpSegment args _ = do
    let interface = argLookup args "interface" "auto"
    let ipv = argLookup args "ipv" 4 :: Int

    let ipvX = case ipv of
                    4 -> show . NetInfo.ipv4
                    6 -> show . NetInfo.ipv6
                    x -> error $ "Unknown protocol: IPv" ++ show x

    intfPred <- case interface of
                     "auto"            -> return $ const True
                     "default_gateway" -> (==) <$> getGatewayInterface
                     intf              -> (==) <$> return intf

    intf <- head
            . sortOn ((*) (-1) . flip (Map.findWithDefault 0) interfacePrefixScores . NetInfo.name)
            . filter (intfPred . NetInfo.name)
            <$> NetInfo.getNetworkInterfaces

    let hlGroup = HighlightGroup "background" Nothing
    return2 $ Segment hlGroup $ ipvX intf

-- powerline.segments.common.net.external_ip
externalIpSegment :: SegmentHandler
externalIpSegment args _ = do
    let interval = argLookup args "interval" 300
    let queryUrl = argLookup args "query_url" "http://ipv4.icanhazip.com"

    fp <- getPowerlineFile "external_ip"

    mtime <- ifM (doesFileExist fp)
                 (Just <$> getModificationTime fp)
                 (return Nothing)

    now <- getCurrentTime
    let doUpdate = maybe True (\m -> diffUTCTime now m > fromRational interval) mtime

    extIP <- if doUpdate
                then do
                    -- TODO: update the file asynchronously, instead of blocking
                    let rstrip = takeWhile $ not . isSpace
                    res <- fmap (rstrip . ASCII.unpack) <$> openURI queryUrl

                    case res of
                         Left  _  -> return Nothing
                         Right ip -> do
                             writeFile fp ip
                             return $ Just ip

                else Just <$> readFile fp

    let hlGroup = HighlightGroup "background" Nothing
    return $ Segment hlGroup <$> maybeToList extIP


-- Assigns a score to an interface, based on its alphabetic prefix
-- Taken from powerline/segments/common.py:74
interfacePrefixScores :: Map.Map String Int
interfacePrefixScores = Map.fromList [
        ("eth",      10),  -- Regular ethernet adapters         : eth1
        ("enp",      10),  -- Regular ethernet adapters, Gentoo : enp2s0
        ("en",       10),  -- OS X                              : en0
        ("ath",       9),  -- Atheros WiFi adapters             : ath0
        ("wlan",      9),  -- Other WiFi adapters               : wlan1
        ("wlp",       9),  -- Other WiFi adapters, Gentoo       : wlp5s0
        ("teredo",    1),  -- miredo interface                  : teredo
        ("lo",      -10),  -- Loopback interface                : lo
        ("docker",   -5),  -- Docker bridge interface           : docker0
        ("vmnet",    -5),  -- VMWare bridge interface           : vmnet1
        ("vboxnet",  -5)   -- VirtualBox bridge interface       : vboxnet0
    ]

-- Returns the interface corresponding to the default route (0.0.0.0/0)
getGatewayInterface :: IO String
getGatewayInterface = head
    . head
    . filter ((==) (0 :: Int) . read . (!! 1))
    . map words
    . drop 1
    . lines
    <$> readFile "/proc/net/route"

-- Combinator for use with functions
applyIf :: Bool -> (a -> a) -> (a -> a)
applyIf True f  = f
applyIf False _ = id


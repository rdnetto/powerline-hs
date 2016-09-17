module Segments.Common.Net (hostnameSegment) where

import Data.Maybe (isJust)
import Network.BSD (getHostName)
import System.Environment (lookupEnv)

import Segments.Base


-- powerline.segments.common.net.hostname
hostnameSegment :: SegmentHandler
hostnameSegment args _ = do
    let onlyIfSsh     = argLookup args "only_if_ssh"   False
    let excludeDomain = argLookup args "excludeDomain" False

    enabled <- if onlyIfSsh
               then isJust <$> lookupEnv "SSH_CLIENT"
               else return True

    let dropDomain = takeWhile (/= '.')

    if enabled
       then return . Segment "hostname" . applyIf excludeDomain dropDomain <$> getHostName
       else return []

-- Combinator for use with functions
applyIf :: Bool -> (a -> a) -> (a -> a)
applyIf True f  = f
applyIf False _ = id


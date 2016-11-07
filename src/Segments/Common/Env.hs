module Segments.Common.Env where

import Data.Maybe (maybeToList)
import System.Environment (lookupEnv)
import System.Posix.User (getEffectiveUserName, getEffectiveUserID)

import Segments.Base


-- powerline.segments.common.env.environment
envSegment :: SegmentHandler
envSegment args _ = do
    let var = argLookup args "variable" ""
    let hlGroup = HighlightGroup "background" Nothing
    value <- lookupEnv var
    return $ maybeToList (Segment hlGroup <$> value)

-- powerline.segments.common.env.user
userSegment :: SegmentHandler
userSegment args _ = do
    let hideUser = argLookup args "hide_user" ""
    let hideDomain = argLookup args "hide_domain" False

    user <- getEffectiveUserName
    isRoot <- (== 0) <$> getEffectiveUserID

    let hlGroup = flip HighlightGroup Nothing $
                    if isRoot
                    then "superuser"
                    else "user"

    let dropDomain = if hideDomain
                        then takeWhile (/= '@')
                        else id

    let user' = if user == hideUser
                   then Nothing
                   else Just $ dropDomain user

    return $ Segment hlGroup <$> maybeToList user'


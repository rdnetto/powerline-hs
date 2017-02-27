{-# LANGUAGE OverloadedStrings #-}

module Segments.VCS where

import Control.Monad (liftM2)
import Data.Aeson as Aeson (Value(Bool, String))
import Data.Aeson.Types (emptyArray)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)

import Aeson.Unpack
import Segments.Base
import Util (readProcess)


-- TODO: use a library instead of reinventing the wheel

-- Represents the status of a file in a generic VCS.
data VcsFileStatus = WorkingDirectoryDirty
                   | IndexDirty
                   | UntrackedFile
                   deriving (Eq, Show)

-- Decodes the blacklist of statuses from the format Powerline uses.
instance ValueType VcsFileStatus where
    unpackValue (String "D") = WorkingDirectoryDirty
    unpackValue (String "I") = IndexDirty
    unpackValue (String "U") = UntrackedFile
    unpackValue x = error $ show x ++ " is not a valid VcsFileStatus"


-- TODO: add support for other VCSs
branchSegment :: SegmentHandler
branchSegment = gitBranchSegment

stashCountSegment :: SegmentHandler
stashCountSegment = simpleHandler "stash" gitStashCount

gitBranchSegment :: SegmentHandler
gitBranchSegment args _ = do
    branch <- gitBranch

    hlGroup <- if   statusColors args
               then branchStatusGroup $ ignoreStatuses args
               else return $ Just "branch"

    return . maybeToList $ liftM2 Segment (flip HighlightGroup Nothing <$> hlGroup) branch

branchStatusGroup :: [VcsFileStatus] -> IO (Maybe String)
branchStatusGroup blacklist = do
    d <- readProcess "git" ["status", "--porcelain"]

    return $ do
        dat <- d

        let fileStatuses = gitToVcsStatus . take 2 <$> lines dat
            isNotMasked = not . flip elem blacklist
            isDirty = const True

        return $ if   any isDirty (filter isNotMasked fileStatuses)
                 then "branch_dirty"
                 else "branch_clean"

-- Converts git status porcelain format to VcsFileStatus
gitToVcsStatus :: String -> VcsFileStatus
gitToVcsStatus "??" = UntrackedFile
gitToVcsStatus [i, ' '] | i /= ' ' = IndexDirty
gitToVcsStatus _ = WorkingDirectoryDirty

gitBranch :: IO (Maybe String)
gitBranch = do
    abbreviated <- readProcess "git" ["rev-parse", "--short", "--abbrev-ref", "HEAD"]

    case abbreviated of
        -- detached branch
        Just "HEAD" -> readProcess "git" ["rev-parse", "--short", "HEAD"]
        x           -> return x

gitStashCount :: IO (Maybe String)
gitStashCount = (=<<) (showNZ . length . lines) <$> readProcess "git" ["stash", "list"] where
    showNZ 0 = Nothing
    showNZ x = Just $ show x

statusColors :: SegmentArgs -> Bool
statusColors = unpackValue . Map.findWithDefault (Bool False) "status_colors"

ignoreStatuses :: SegmentArgs -> [VcsFileStatus]
ignoreStatuses = unpackValue . Map.findWithDefault emptyArray "ignore_statuses"


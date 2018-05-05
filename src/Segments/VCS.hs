{-# LANGUAGE OverloadedStrings #-}

module Segments.VCS where

import Control.Monad (liftM2)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
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

gitCommitsAheadCountSegment :: SegmentHandler
gitCommitsAheadCountSegment = simpleHandler "gitstatus_ahead" gitCommitsAheadCount

gitCommitsBehindCountSegment :: SegmentHandler
gitCommitsBehindCountSegment = simpleHandler "gitstatus_behind" gitCommitsBehindCount

gitStagedFilesCountSegment :: SegmentHandler
gitStagedFilesCountSegment = simpleHandler "gitstatus_staged" gitStagedFilesCount

gitUnmergedFilesCountSegment :: SegmentHandler
gitUnmergedFilesCountSegment = simpleHandler "gitstatus_unmerged" gitUnmergedFilesCount

gitChangedFilesCountSegment :: SegmentHandler
gitChangedFilesCountSegment = simpleHandler "gitstatus_changed" gitChangedFilesCount

gitUntrackedFilesCountSegment :: SegmentHandler
gitUntrackedFilesCountSegment = simpleHandler "gitstatus_untracked" gitUntrackedFilesCount

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

gitCommitsAheadCount :: IO (Maybe String)
gitCommitsAheadCount = runMaybeT $ do
  branch <- MaybeT gitBranch
  remote <- MaybeT $ readProcess "git" ["config", "--get", "branch." ++ branch ++ ".remote"]
  count  <- MaybeT $ readProcess "git" ["rev-list", "--count", remote ++ "/" ++ branch ++ ".." ++ branch]
  MaybeT . return . showNZ . read $ count

gitCommitsBehindCount :: IO (Maybe String)
gitCommitsBehindCount = runMaybeT $ do
  branch <- MaybeT gitBranch
  remote <- MaybeT $ readProcess "git" ["config", "--get", "branch." ++ branch ++ ".remote"]
  count  <- MaybeT $ readProcess "git" ["rev-list", "--count", branch ++ ".." ++ remote ++ "/" ++ branch]
  MaybeT . return . showNZ . read $ count

gitStagedFilesCount :: IO (Maybe String)
gitStagedFilesCount = runMaybeT $ do
  count <- MaybeT $ readProcess "git" ["diff", "--staged", "--numstat"]
  MaybeT . return . showNZ . length . lines $ count

gitUnmergedFilesCount :: IO (Maybe String)
gitUnmergedFilesCount = runMaybeT $ do
  count <- MaybeT $ readProcess "git" ["diff", "--diff-filter=U", "--numstat"]
  MaybeT . return . showNZ . length . lines $ count

gitChangedFilesCount :: IO (Maybe String)
gitChangedFilesCount = runMaybeT $ do
  count <- MaybeT $ readProcess "git" ["status", "--porcelain"]
  MaybeT . return . showNZ . length . lines $ count

gitUntrackedFilesCount :: IO (Maybe String)
gitUntrackedFilesCount = runMaybeT $ do
  count <- MaybeT $ readProcess "git" ["ls-files", "--other", "--exclude-standard"]
  MaybeT . return . showNZ . length . lines $ count

gitStashCount :: IO (Maybe String)
gitStashCount = runMaybeT $ do
  count <- MaybeT $ readProcess "git" ["stash", "list"]
  MaybeT . return . showNZ . length . lines $ count

showNZ :: Int -> Maybe String
showNZ 0 = Nothing
showNZ x = Just $ show x

statusColors :: SegmentArgs -> Bool
statusColors = unpackValue . Map.findWithDefault (Bool False) "status_colors"

ignoreStatuses :: SegmentArgs -> [VcsFileStatus]
ignoreStatuses = unpackValue . Map.findWithDefault emptyArray "ignore_statuses"


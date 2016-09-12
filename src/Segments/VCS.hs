module Segments.VCS where

import Control.Monad (liftM2)
import Data.Aeson as Aeson (Value(Bool))
import Data.Aeson.Types (emptyArray)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)

import Aeson_Unpack
import Debug.Trace
import Segments.Base
import Util (readProcess)

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

    return . maybeToList $ liftM2 Segment hlGroup branch

branchStatusGroup :: [String] -> IO (Maybe String)
branchStatusGroup blacklist = do
    d <- readProcess "git" ["status", "--porcelain"]

    return $ do
        dat <- d

        let fileStatuses = head . words <$> lines dat
            isNotMasked s = not $ any (`isInfixOf` s) blacklist
            isDirty = const True

        return $ if   any isDirty (filter isNotMasked fileStatuses)
                 then "branch_dirty"
                 else "branch_clean"

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

ignoreStatuses :: SegmentArgs -> [String]
ignoreStatuses = unpackValue . Map.findWithDefault emptyArray "ignore_statuses"


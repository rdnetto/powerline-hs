module Segments.VCS(branchSegment, stashCountSegment) where

import Segments.Base
import Util (readProcess)

-- TODO: add support for other VCSs
-- TODO: hlGroup should depend on branch status (if appropriate args set)
branchSegment :: SegmentHandler
branchSegment = simpleHandler "branch" gitBranch

stashCountSegment :: SegmentHandler
stashCountSegment = simpleHandler "stash" gitStashCount

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


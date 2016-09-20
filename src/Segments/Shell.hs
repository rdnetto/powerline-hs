module Segments.Shell where

import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath, splitPath, dropTrailingPathSeparator)

import Aeson_Unpack
import CommandArgs
import Segments.Base
import Util


-- powerline.segments.shell.last_status
lastStatusSegment :: SegmentHandler
lastStatusSegment _ args = return2 . statusSegment . last $ lastPipeStatus args

-- powerline.segments.shell.last_pipe_status
pipeStatusSegment :: SegmentHandler
pipeStatusSegment _ args = return $ statusSegment <$> lastPipeStatus args

-- Common logic for exit code segments
statusSegment :: Int -> Segment
statusSegment 0 = Segment "exit_success" "0"
statusSegment x = Segment "exit_fail" (show x)

-- powerline.segments.shell.cwd
cwdSegment :: SegmentHandler
cwdSegment args ctx = do
    let argPath = Map.lookup "shortened_path" $ rendererArgs ctx
    -- Path is normally provided via rendererArgs, but fallback to syscall if its not
    cwd <- maybe getCurrentDirectory return argPath
    let pathComponents = dropTrailingPathSeparator <$> splitPath cwd

    -- Truncate parent components (iff set)
    let truncateComponent = case maxParentLen args of
                                Just len -> take len
                                Nothing  -> id

    let truncateParentComponents = joinEnd . mapFst (map truncateComponent) . splitEnd

    -- Truncate list of components
    let e = maybeToList $ ellipsis args
    let applyDepthLimit = case depthLimit args of
                                Just n  -> takeEnd n e
                                Nothing -> id

    -- If combineSegs, use a single segment instead of multiple
    let pathComponents' = applyDepthLimit $ truncateParentComponents pathComponents

    return $ Segment "cwd" <$> (
            if   combineSegs args
            then return $ joinPath pathComponents'
            else pathComponents'
        )

-- powerline.segments.shell.jobnum
jobNumSegment :: SegmentHandler
jobNumSegment args ctx = do
    let showZero = argLookup args "show_zero" False
    let val = jobNum ctx

    if val == 0 && not showZero
       then return []
       else return2 . Segment "jobnum" $ show val

-- powerline.segments.shell.mode
modeSegment :: SegmentHandler
modeSegment args ctx = do
    let defaultModes = catMaybes [
                Map.lookup "default_mode" (rendererArgs ctx),
                argLookup' args "default"
            ]
    let overrideDict = argLookup args "override" $ Map.fromList [
                ("vicmd", "COMMND"),
                ("viins", "INSERT")
            ]

    let mode = Map.lookup "mode" (rendererArgs ctx)

    let ignoreMode m | m `elem` defaultModes = Nothing
                     | otherwise             = Just m

    let overrideMode m = fromMaybe m $ Map.lookup m overrideDict

    return . maybeToList $ do
        m <- ignoreMode =<< mode
        return . Segment "mode" $ overrideMode m

-- Truncate parent components to this length
maxParentLen :: SegmentArgs -> Maybe Int
maxParentLen args = unpackValue <$> Map.lookup "dir_shorten_len" args

-- Only show this many path components
depthLimit :: SegmentArgs -> Maybe Int
depthLimit args = unpackValue <$> Map.lookup "dir_limit_depth" args

-- If true, use a single segment instead of splitting them
combineSegs :: SegmentArgs -> Bool
combineSegs args = fromMaybe False $ unpackValue <$> Map.lookup "use_path_seperator" args

-- Substitute for omitted components. Omitted if present and null.
ellipsis :: SegmentArgs -> Maybe String
ellipsis args = res where
    res = case Map.lookup  "ellipsis" args of
               Nothing   -> Just "⋯"
               Just Null -> Nothing
               Just val  -> Just $ unpackValue val

-- Returns the last n elements in xs, with e prepended.
-- If |xs| >= n, e is not prepended.
takeEnd :: Int -> [a] -> [a] -> [a]
takeEnd n e xs | l > n     = e ++ xs'
               | otherwise = xs
    where
        l = length xs
        xs' = drop (l - n) xs


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rendering(renderSegment) where

import qualified Data.Map.Lazy as Map
import Prelude hiding (lookup)
import Rainbow

import qualified ConfigSchema as CS
import Segments.Base (Segment(..))

type ChunkFormatter = Chunk String -> Chunk String

-- Render a segment
renderSegment :: CS.ColourDict -> CS.ColourScheme -> Segment -> Chunk String
renderSegment colourDict colourScheme Segment{..} = res where
    hlGroup = segmentGroup

    fmt = if   hlGroup == ""
          then id
          else formatChunk colourDict $ colourScheme `lookup` hlGroup

    res = fmt $ chunk segmentText


formatChunk :: CS.ColourDict -> CS.TerminalColour -> ChunkFormatter
formatChunk colourDict CS.TerminalColour {..} = fg' . bg' . attrs' where
    fg' = fore . toRadiant $ colourDict `lookup` fg
    bg' = back . toRadiant $ colourDict `lookup` bg
    attrs' = foldl (.) id $ toChunkFormatter <$> attrs

lookup :: Map.Map String v -> String -> v
lookup m k = case Map.lookup k m of
                   Just x  -> x
                   Nothing -> error $ "Unknown key: " ++ show k


-- Converts a colour from the format used in ConfigSchema to Rainbow's representation
-- TODO: add support for 24-bit colour. Note that powerline already has a config option for enabling 24-bit color, so this is purely a matter of emitting the right escape codes.
toRadiant :: CS.Colour -> Radiant
toRadiant (CS.CtermColour x)  = color256 x
toRadiant (CS.TrueColour x _) = color256 x

toChunkFormatter :: String -> ChunkFormatter
toChunkFormatter "bold" = bold
toChunkFormatter "italic" = italic
toChunkFormatter "underline" = underline
toChunkFormatter x = error $ "Unknown font attribute: " ++ x


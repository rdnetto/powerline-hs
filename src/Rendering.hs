{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rendering(putChunks, renderSegments, RenderInfo(RenderInfo)) where

import qualified Data.ByteString as BS
import Data.Function (on)
import qualified Data.Map.Lazy as Map
import Prelude hiding (lookup)
import Rainbow

import qualified ConfigSchema as CS
import Segments.Base (Segment(..), modifySegText)
import Util

type ChunkFormatter = Chunk String -> Chunk String
type RainbowRenderer a = Chunk a -> [ByteString] -> [ByteString]

data RenderInfo = RenderInfo {
    colourDict   :: CS.ColourDict,
    colourScheme :: CS.ColourScheme,
    dividers     :: CS.ForBothSides CS.Divider,
    numSpaces    :: Int
}

-- Render a segment
renderSegment :: RenderInfo -> Segment -> Chunk String
renderSegment RenderInfo{..} Segment{..} = res where
    hlGroup = segmentGroup

    fmt = if   hlGroup == ""
          then id
          else formatChunk colourDict $ colourScheme `lookup` hlGroup

    res = fmt . chunk $ segmentText

-- Render the segments, and the dividers between them.
renderSegments :: RenderInfo -> Side -> [Segment] -> [Chunk String]
renderSegments rInfo@RenderInfo{..} s segments = res where
    {-
     - Dividers:
     - * Segments on the left side have *numSpaces* to their right, and vice versa for segments on the right.
     - * Dividers have their own styling which is different to that of the segments on either side
     -}

    -- select the divider - hard for different background colours, soft for the same. (Note that this is based on background colour only, not the style itself)
    divCfg = dividers & side CS.left CS.right s
    sGroupEq = (==) `on` segmentGroup
    chooseDiv x y | x `sGroupEq` y = x { segmentText = CS.soft divCfg }
                  | otherwise      = x { segmentText = CS.hard divCfg }

    pad = appendSide (oppositeSide s) (replicate numSpaces ' ')
    res = renderSegment rInfo . modifySegText pad <$> segments

-- Helper method for rendering chunks
putChunks :: RainbowRenderer a -> [Chunk a] -> IO ()
putChunks renderer = mapM_ BS.putStr . chunksToByteStrings renderer

formatChunk :: CS.ColourDict -> CS.TerminalColour -> ChunkFormatter
formatChunk colourDict CS.TerminalColour {..} = fg' . bg' . attrs' where
    fg' = fore . toRadiant $ colourDict `lookup` fg
    bg' = back . toRadiant $ colourDict `lookup` bg
    attrs' = foldl (.) id $ toChunkFormatter <$> attrs

lookup :: Map.Map String v -> String -> v
lookup m k = case Map.lookup k m of
                   Just x  -> x
                   Nothing -> error $ "Unknown key: " ++ show k

-- Appends the first list to the specified side of the second.
appendSide :: Side -> [a] -> [a] -> [a]
appendSide SLeft  = (++)
appendSide SRight = flip (++)

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


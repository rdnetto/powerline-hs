{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rendering(putChunks, renderSegments, RenderInfo(RenderInfo)) where

import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Prelude hiding (lookup)
import Rainbow

import qualified ConfigSchema as CS
import Segments.Base (Segment(..), modifySegText)
import Util

-- Applies formatting to a Chunk
type ChunkFormatter = Chunk String -> Chunk String

-- Convenience type for a function which renders Chunk diff lists to ByteStrings
type RainbowRenderer a = Chunk a -> [ByteString] -> [ByteString]

data RenderInfo = RenderInfo {
    colourDict   :: CS.ColourDict,
    colourScheme :: CS.ColourScheme,
    dividers     :: CS.ForBothSides CS.Divider,
    numSpaces    :: Int
}

-- Render a segment
renderSegment :: RenderInfo -> Segment -> Chunk String
renderSegment rinfo@RenderInfo{..} Segment{..} = res where
    hlGroup = segmentGroup

    fmt = if   hlGroup == ""
          then id
          else formatChunk colourDict . fromJust $ lookupStyle rinfo hlGroup

    res = fmt . chunk $ segmentText
renderSegment _ Divider{..} = res where
    res = fore divFore . back divBack $ chunk divText

-- Renders a prompt of segments, including the required spaces and padding
renderSegments :: RenderInfo -> Side -> [Segment] -> [Chunk String]
renderSegments rInfo@RenderInfo{..} s segments = res where
    -- TODO: the last segment should have a divider after it as well, using background and background:divider styling

    makeDiv x y = Divider{..} where
            -- The previous segment is the one closer to the side of the screen we started at
            prev = side x y s
            next = side y x s

            hlTuple seg = styleTuple rInfo $ lookupStyle rInfo (segmentGroup seg) `withDef` lookupStyle rInfo "background"
            (_, prevBack) = hlTuple prev
            (_, nextBack) = hlTuple next

            -- Foreground and backround colour are taken from the background colours of the adjacent segments
            -- TODO: this can be overriden with an explicit style for certain segments
            divFore = prevBack
            divBack = nextBack

            -- Use hard dividers when the background colours are different, soft when they're the same.
            -- Note that this is a function of the background colour itself, not just the name.
            divType = if   prevBack == nextBack
                      then CS.soft
                      else CS.hard
            divText = dividers & side CS.left CS.right s & divType


    insertDivs = intersperseBy makeDiv

    -- Padding: segments on the left side have *numSpaces* to their right, and vice versa for segments on the right.
    pad = appendSide (oppositeSide s) (replicate numSpaces ' ')

    -- We need to add a space to the very first / last segment as a special case (dependng on which side we're on)
    appendSpace = (++ " ")
    prependSpace = (' ':)
    padEnd = side (mapFirst $ modifySegText prependSpace) (mapLast $ modifySegText appendSpace) s

    res = map (renderSegment rInfo) . insertDivs . map (modifySegText pad) . padEnd $ segments

-- Helper method for rendering chunks
putChunks :: RainbowRenderer a -> [Chunk a] -> IO ()
putChunks renderer = mapM_ BS.putStr . chunksToByteStrings renderer

formatChunk :: CS.ColourDict -> CS.TerminalColour -> ChunkFormatter
formatChunk colourDict CS.TerminalColour {..} = fg' . bg' . attrs' where
    fg' = fore . toRadiant $ Map.lookup fg colourDict `withDef` error ("Unknown colour: " ++ fg)
    bg' = back . toRadiant $ Map.lookup bg colourDict `withDef` error ("Unknown colour: " ++ bg)
    attrs' = foldl (.) id $ toChunkFormatter <$> attrs

lookupStyle :: RenderInfo -> String -> Maybe CS.TerminalColour
lookupStyle RenderInfo{..} k = Map.lookup k colourScheme

-- Converts a TerminalColour to a (foreground, background) tuple
styleTuple :: RenderInfo -> CS.TerminalColour -> (Radiant, Radiant)
styleTuple RenderInfo{..} CS.TerminalColour{..} = mapBoth f (fg, bg) where
    f x = toRadiant $ Map.lookup x colourDict `withDef` Map.lookup "background" colourDict

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


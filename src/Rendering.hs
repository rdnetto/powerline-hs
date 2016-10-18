module Rendering(putChunks, renderSegments, RenderInfo(RenderInfo)) where

import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust, catMaybes)
import Prelude hiding (lookup, div)
import Rainbow
import Safe

import qualified ConfigSchema as CS
import Segments.Base (GradientWeight, Segment(..), HighlightGroup(..), modifySegText)
import Util

-- Applies formatting to a Chunk
type ChunkFormatter = Chunk String -> Chunk String

-- Convenience type for a function which renders Chunk diff lists to ByteStrings
type RainbowRenderer a = Chunk a -> [ByteString] -> [ByteString]

data RenderInfo = RenderInfo {
    colourConfig :: CS.ColourConfig,
    colourScheme :: CS.ColourScheme,
    dividers     :: CS.ForBothSides CS.Divider,
    numSpaces    :: Int
} deriving Show

-- Render a segment
renderSegment :: RenderInfo -> Segment -> Chunk String
renderSegment rinfo@RenderInfo{..} Segment{..} = res where
    HighlightGroup hGroup gradientWeight = segmentGroup

    fmt = if   hGroup == ""
          then id
          else formatChunk colourConfig gradientWeight . fromJust $ lookupStyle rinfo segmentGroup

    res = fmt . chunk $ segmentText
renderSegment _ Divider{..} = res where
    res = fore divFore . back divBack $ chunk divText

-- Renders a prompt of segments, including the required spaces and padding
renderSegments :: RenderInfo -> Side -> [Segment] -> [Chunk String]
renderSegments rInfo@RenderInfo{..} s segments = res where
    -- NOTE: the equivalent logic is in powerline/renderer.py:540
    makeDiv x y = div where
            -- The previous segment is the one closer to the side of the screen we started at
            prev = side x y s
            next = side y x s

            -- Use hard dividers when the background colours are different, soft when they're the same.
            -- Note that this is a function of the background colour itself, not just the name.
            isSoft  = prevBack == nextBack
            divType = if   isSoft
                      then CS.soft
                      else CS.hard
            divText = dividers & side CS.left CS.right s & divType

            -- Only soft dividers can use explicit styling
            div = if   isSoft
                  then Divider styleFore styleBack divText
                  else Divider divFore   divBack   divText

            -- Use explicit styling from previous segment
            hlSegGroup = hlGroup . segmentGroup
            simpleHGroup n = HighlightGroup n Nothing

            (styleFore, styleBack) = styleTuple rInfo . headNote ("looking up segmentGroup " ++ show (segmentGroup prev)) . catMaybes $ lookupStyle rInfo <$> [
                    simpleHGroup $ hlSegGroup prev ++ ":divider",    -- segment[segment["divider_highlight_group"] = "time:divider"]
                    simpleHGroup $ hlSegGroup prev                   -- Fallback to the normal styling for that segment
                ]

            -- Lookup style from adjacent segments
            hlTuple seg = styleTuple rInfo $ lookupStyleWithFallback rInfo (segmentGroup seg) (simpleHGroup "background")
            (_, prevBack) = hlTuple prev
            (_, nextBack) = hlTuple next

            -- Foreground and background colour are taken from the background colours of the adjacent segments for hard dividers
            divFore = prevBack
            divBack = nextBack



    -- Insert dividers between segments
    insertDivs = intersperseBy makeDiv

    -- Special case to add dividers to the ends.
    addEndDiv xs = let bg = Segment (HighlightGroup "background" Nothing) ""
                   in case s of
                           SLeft  -> xs ++ [makeDiv (last xs) bg]
                           SRight -> makeDiv bg (head xs) : xs

    -- Padding: segments on the left side have *numSpaces* to their right, and vice versa for segments on the right.
    pad = appendSide (oppositeSide s) (replicate numSpaces ' ')

    -- We need to add a space to the very first / last segment as a special case (dependng on which side we're on)
    appendSpace = (++ " ")
    prependSpace = (' ':)
    padEnd = side (mapFirst $ modifySegText prependSpace) (mapLast $ modifySegText appendSpace) s

    res = map (renderSegment rInfo) . addEndDiv . insertDivs . map (modifySegText pad) . padEnd $ segments

-- Helper method for rendering chunks
putChunks :: RainbowRenderer a -> [Chunk a] -> IO ()
putChunks renderer = mapM_ BS.putStr . chunksToByteStrings renderer

formatChunk :: CS.ColourConfig -> Maybe GradientWeight -> CS.TerminalColour -> ChunkFormatter
formatChunk CS.ColourConfig{..} gradWeight CS.TerminalColour{..} = fg' . bg' . attrs' where
    -- name can be either the name of a colour or a gradient - depends on gradWeight
    lookupCol name = case gradWeight of
                       Nothing -> fromJustNote ("Unknown colour: " ++ name) $ Map.lookup name colourDict
                       Just gw -> head $ catMaybes [
                                    -- If a gradient doesn't exist with this name, it's probably a colour
                                    -- This is needed because the gradient is typically used for the one, but not both, of fg/bg
                                    applyWeight gw <$> Map.lookup name gradientDict,
                                    Map.lookup name colourDict,
                                    Just $ error ("Unknown gradient/colour: " ++ name)
                                  ]

    fg' = fore . toRadiant $ lookupCol fg
    bg' = back . toRadiant $ lookupCol bg
    attrs' = foldl (.) id $ toChunkFormatter <$> attrs

-- Resolves a gradient weight and a gradient to a colour
applyWeight :: GradientWeight -> CS.ColourGradient -> CS.Colour
applyWeight gw (CS.CtermGradient xs)   = CS.CtermColour (pick gw xs)
applyWeight gw (CS.TrueGradient xs ys) = CS.TrueColour (pick gw xs) (pick gw ys)

lookupStyle :: RenderInfo -> HighlightGroup -> Maybe CS.TerminalColour
lookupStyle RenderInfo{..} (HighlightGroup k _) = Map.lookup k colourScheme

unsafeLookupStyle :: RenderInfo -> HighlightGroup -> CS.TerminalColour
unsafeLookupStyle RenderInfo{..} (HighlightGroup k _) = res where
    res = case Map.lookup k colourScheme of
               Just x  -> x
               Nothing -> error $ "Could not find style with name " ++ k

lookupStyleWithFallback :: RenderInfo -> HighlightGroup -> HighlightGroup -> CS.TerminalColour
lookupStyleWithFallback rInfo s fb = res where
    res = head $ catMaybes [
            lookupStyle rInfo s,
            lookupStyle rInfo fb,
            error $ "Could not find styles with names " ++ show [s, fb]
        ]

-- Converts a TerminalColour to a (foreground, background) tuple
styleTuple :: RenderInfo -> CS.TerminalColour -> (Radiant, Radiant)
styleTuple rInfo CS.TerminalColour{..} = (f fg defaultFg, f bg defaultBg) where
    f x def = toRadiant $ lookupCol x `withDef` lookupCol def
    lookupCol name = Map.lookup name colDict
    colDict = CS.colourDict $ colourConfig rInfo
    CS.TerminalColour defaultFg defaultBg _ = unsafeLookupStyle rInfo $ HighlightGroup "background" Nothing

-- Appends the first list to the specified side of the second.
appendSide :: Side -> [a] -> [a] -> [a]
appendSide SLeft  = (++)
appendSide SRight = flip (++)

-- Converts a colour from the format used in ConfigSchema to Rainbow's representation
-- TODO: add support for 24-bit colour. Note that powerline already has a config option for enabling 24-bit color, so this is purely a matter of emitting the right escape codes.
toRadiant :: CS.Colour -> Radiant
toRadiant (CS.CtermColour x)  = color256 x
toRadiant (CS.TrueColour x _) = color256 x

-- Selects the (N*f)th element in a list
pick :: Float -> [a] -> a
pick f xs = xs !! round i where
    i = f * fromIntegral (length xs)

toChunkFormatter :: String -> ChunkFormatter
toChunkFormatter "bold" = bold
toChunkFormatter "italic" = italic
toChunkFormatter "underline" = underline
toChunkFormatter x = error $ "Unknown font attribute: " ++ x


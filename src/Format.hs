{-# LANGUAGE ScopedTypeVariables #-}

-- This module is a compatibility layer that emulates the behaviour of Python's format strings.
-- It is needed because several Powerline segments take a format argument that uses this notation.
-- Interpolation sections have the grammar {[name][:fmt]}, and may also contain arbitrary text.
--
-- See: https://docs.python.org/3.3/library/string.html#format-specification-mini-language

module Format where

import Data.Char (isDigit)
import Safe (lastMay)
import Text.ParserCombinators.ReadP


type PyFormatStr = String   -- format used by Python's str.format()
type HsFormatStr = String   -- format used by Text.Printf

-- Grammar: [[fill]align][sign][#][0][width][,][.precision][type]
data FormatSegment = FormatStr {
                            fillAlign       :: Maybe FillAlign,
                            numbericSign    :: Maybe NumericSign,
                            formatWidth     :: Maybe Int,
                            formatPrecision :: Maybe Int,
                            formatChar      :: Maybe Char
                           }
                   | TextStr String
                   deriving (Eq, Show)

data FillAlign = FillAlign Char Alignment
               deriving (Eq, Show)

data Alignment = LeftAlign      -- <
               | RightAlign     -- >
               | SignAlign      -- =
               | CentredAlign   -- ^
               deriving (Eq, Show)

data NumericSign = Always       -- +
                 | OnlyNegative -- -
                 | SpaceSign    -- ' '
               deriving (Eq, Show)


parseFmt :: PyFormatStr -> [FormatSegment]
parseFmt fmt = res where
    res = case lastMay $ readP_to_S parser fmt of
            Just (x, "") -> x
            _            -> fail $ '\'' : fmt ++ "' does not have a recognized format."

    bracketParser = between (char '{') (char '}') fmtParser
    textParser = TextStr <$> munch1 (/= '{')
    parser :: ReadP [FormatSegment] = many1 $ bracketParser <++ textParser

    -- Grammar: [[fill]align][sign][#][0][width][,][.precision][type]
    fillChar = satisfy (not . flip elem "<>=^") +++ return ' '
    fillAlignParser = maybeParse (FillAlign <$> fillChar <*> alignParser)

    alignParser = mapParser [
            ('<', LeftAlign),
            ('>', RightAlign),
            ('=', SignAlign),
            ('^', CentredAlign)
        ]

    signParser = maybeParse $ mapParser [
            ('+', Always),
            ('-', OnlyNegative),
            (' ', SpaceSign)
        ]

    -- Note that we are not implementing these for now
    ignoredFlags1 = optional (char '#') <* optional (char '0')
    ignoredFlags2 = optional (char ',')

    widthParser = maybeParse parseInt
    precisionParser = maybeParse $ char '.' >> parseInt

    typeParser = maybeParse . satisfy $ flip elem [
            -- These are extensions that are present in Python but not printf
            -- 'b',        -- binary
            -- 'n',        -- like %i, but with 1000s separators
            -- '%'         -- percentage

            'c',        -- character
            'd',        -- decimal
            'e', 'E',   -- floats
            'f', 'F',
            'g', 'G',
            'o',        -- octal
            's',        -- string
            'x', 'X'    -- hexadecimal
        ]

    fmtParser = FormatStr <$> fillAlignParser
                          <*> signParser <* ignoredFlags1
                          <*> widthParser <* ignoredFlags2
                          <*> precisionParser
                          <*> typeParser

-- Parsers a key in the map into the corresponding value, or fails.
mapParser :: [(Char, b)] -> ReadP b
mapParser dict = do
    c <- get

    case lookup c dict of
         Just x  -> return x
         Nothing -> pfail

-- Parses an unsigned int
parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

convFmt :: [FormatSegment] -> HsFormatStr
convFmt = undefined

maybeParse :: ReadP a -> ReadP (Maybe a)
maybeParse p = option Nothing (Just <$> p)

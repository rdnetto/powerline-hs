{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- This module is a compatibility layer that emulates the behaviour of Python's format strings.
-- It is needed because several Powerline segments take a format argument that uses this notation.
-- Interpolation sections have the grammar {[name][:fmt]}, and may also contain arbitrary text.
--
-- See: https://docs.python.org/3.3/library/string.html#format-specification-mini-language

module Format (pyFormat) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Safe (lastMay)
import Text.ParserCombinators.ReadP
import Text.Printf (printf, PrintfType)

import Util


type PyFormatStr = String   -- format used by Python's str.format()
type HsFormatStr = String   -- format used by Text.Printf

-- Grammar: [[fill]align][sign][#][0][width][,][.precision][type]
data FormatSegment = FormatStr {
                            fillAlign       :: Maybe FillAlign,
                            numericSign     :: Maybe NumericSign,
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

-- Entry point: converts the format String and calls printf with it
pyFormat :: PrintfType r => PyFormatStr -> r
pyFormat = printf . concatMap convertFmt . parseFmt

parseFmt :: PyFormatStr -> [FormatSegment]
parseFmt fmt = res where
    res = case lastMay $ readP_to_S parser fmt of
            Just (x, "") -> x
            _            -> fail $ '\'' : fmt ++ "' does not have a recognized format."

    textParser = TextStr <$> munch1 (/= '{')
    parser :: ReadP [FormatSegment] = many1 $ fmtParser <++ textParser

    -- "{" [field_name] ["!" conversion] [":" format_spec] "}"
    fmtParser = between (char '{') (char '}') $ nameParser *> convParser *> fmtSpec
    nameParser = skipMany . satisfy $ const True
    convParser = optional $ char '!' *> (char 'r' +++ char 's' +++ char 'a')
    fmtSpec = option defFmtSpec $ char ':' *> fmtSpecParser
    defFmtSpec = FormatStr Nothing Nothing Nothing Nothing Nothing

    -- Format Spec Grammar: [[fill]align][sign][#][0][width][,][.precision][type]
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

    fmtSpecParser = FormatStr <$> fillAlignParser
                              <*> signParser
                              <*  ignoredFlags1
                              <*> widthParser
                              <*  ignoredFlags2
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

maybeParse :: ReadP a -> ReadP (Maybe a)
maybeParse p = option Nothing (Just <$> p)

convertFmt :: FormatSegment -> HsFormatStr
convertFmt (TextStr s) = replace "%" "%%" s
convertFmt FormatStr{..} = res where
    res = concatMap (fromMaybe "") [
            Just "%",
            convAlign <$> fillAlign,
            convSign <$> numericSign,
            show <$> formatWidth,
            ('.':) . show <$> formatPrecision,
            return <$> formatChar `orElse` Just 'v'
        ]

convAlign :: FillAlign -> String
convAlign (FillAlign ' ' LeftAlign)  = "-"
convAlign (FillAlign ' ' RightAlign) = ""
convAlign (FillAlign '0' a) = '0' : convAlign (FillAlign ' ' a)
convAlign fa = error $ "Unsupported alignment format with: " ++ show fa

convSign :: NumericSign -> String
convSign Always = "+"
convSign SpaceSign = " "
convSign OnlyNegative = ""


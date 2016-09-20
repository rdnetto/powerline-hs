{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigSchema where

import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map.Lazy as MapL
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific
import Data.Text (Text)
import Data.Vector as V
import Data.Word (Word8)
import GHC.Generics (Generic)

import Aeson_Unpack

-- type alias for entries we don't care about
type DontCare = Object

-- config.json

type CommonConfig = Map String Value

data MainConfig = MainConfig {
    common :: CommonConfig,
    ext    :: ExtConfigs
} deriving (Generic, Show)
instance FromJSON MainConfig

data ExtConfigs = ExtConfigs {
    ipython :: ExtConfig,
    shell   :: ExtConfig,
    tmux    :: ExtConfig,
    vim     :: ExtConfig
} deriving (Generic, Show)
instance FromJSON ExtConfigs

data ExtConfig = ExtConfig {
    colorscheme :: String,
    theme       :: String
} deriving (Generic, Show)
instance FromJSON ExtConfig

defaultTopTheme :: CommonConfig -> String
defaultTopTheme cfg = fromMaybe def val where
    -- TODO: this should default to 'powerline' for UTF-8 locales and 'ascii' for others, but locale detection is non-trivial
    def = "powerline"
    val = unpackValue <$> MapL.lookup "default_top_theme" cfg


-- colors.json
type ColourDict = Map String Colour

data ColourConfig = ColourConfig {
    colors :: ColourDict,
    gradients :: DontCare
} deriving (Generic, Show)
instance FromJSON ColourConfig

-- Colours can be represented as the cterm code with an optional 24-bit hex representation. e.g. [1, "#AABBCC"]
data Colour = CtermColour Word8
            | TrueColour Word8 Text
            deriving (Generic, Show)

instance FromJSON Colour where
    parseJSON x@(Number _) = withScientific "Colour" f x where
        f n | isInteger n && inRange n = return . CtermColour . fromJust $ toBoundedInteger n
            | otherwise                = typeMismatch "Colour" x
        inRange n = 0 <= n && n < 256
    parseJSON (Array arr) =
        case toList arr of
            [Number c, String h] -> return $ TrueColour (fromJust $ toBoundedInteger c) h
            _                    -> typeMismatch "Colour" (Array arr)
    parseJSON invalid = typeMismatch "Colour" invalid

-- colorschemes/*.json

type ColourScheme = Map String TerminalColour

data ColourSchemeConfig = ColourSchemeConfig {
    groups :: ColourScheme,
    modeTranslations :: Map String ColourSchemeConfig
} deriving (Generic, Show)

instance FromJSON ColourSchemeConfig where
    parseJSON (Object obj) = ColourSchemeConfig <$> obj .:  "groups"
                                                <*> obj .:? "mode_translations" .!= MapL.empty
    parseJSON invalid      = typeMismatch "ColourSchemeConfig" invalid

data TerminalColour = TerminalColour {
    fg :: String,
    bg :: String,
    attrs :: [String]
} deriving (Generic, Show, Eq)
instance FromJSON TerminalColour

-- themes/*.json

data ThemeConfig = ThemeConfig {
    default_module  :: Maybe DontCare,
    spaces          :: Maybe Int,
    dividers        :: Maybe (ForBothSides Divider),
    segments        :: ForBothSides [Segment],
    segment_data    :: Map String SegmentData
} deriving (Generic, Show)
instance FromJSON ThemeConfig

data ForBothSides a = ForBothSides {
    left    :: a,
    right   :: a
} deriving (Generic, Show, Functor)
instance FromJSON a => FromJSON (ForBothSides a)

data Divider = Divider {
    hard :: String,
    soft :: String
} deriving (Generic, Show)
instance FromJSON Divider

data Segment = Segment {
    function    :: String,
    before      :: Maybe String,
    after       :: Maybe String,
    args        :: Maybe SegmentArgs
} deriving (Generic, Show)
instance FromJSON Segment

-- Like Segment, but used in a different context.
data SegmentData = SegmentData {
    sdBefore    :: Maybe String,
    sdAfter     :: Maybe String,
    sdArgs      :: Maybe SegmentArgs
} deriving (Generic, Show)

-- Explicit definition needed because we can't have duplicate record fields until GHC 8.0
instance FromJSON SegmentData where
    parseJSON (Object obj) = SegmentData <$> obj .:? "before"
                                         <*> obj .:? "after"
                                         <*> obj .:? "args"
    parseJSON invalid      = typeMismatch "SegmentData" invalid

type SegmentArgs = Map String Value

-- Helper function that combines lookup and type conversion.
argLookup :: ValueType a => SegmentArgs -> String -> a -> a
argLookup sa k def = res where
    res = case MapL.lookup k sa of
            Just x  -> unpackValue x
            Nothing -> def

-- Like argLookup, but exposes failure.
argLookup' :: ValueType a => SegmentArgs -> String -> Maybe a
argLookup' sa k = unpackValue <$> MapL.lookup k sa


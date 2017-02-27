{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Schema where

import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map.Lazy as MapL
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Vector as V hiding ((++))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Safe (fromJustNote)

import Aeson.Unpack

-- type alias for entries we don't care about
type DontCare = Value

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
    theme       :: String,
    localThemes :: Map String String    -- specifies the theme to use for a specific context
} deriving (Generic, Show)

instance FromJSON ExtConfig where
    parseJSON (Object obj) = ExtConfig <$> obj .:  "colorscheme"
                                       <*> obj .:  "theme"
                                       <*> obj .:? "local_themes" .!= MapL.empty
    parseJSON invalid = typeMismatch "ExtConfig" invalid

defaultTopTheme :: CommonConfig -> String
defaultTopTheme cfg = fromMaybe def val where
    -- TODO: this should default to 'powerline' for UTF-8 locales and 'ascii' for others, but locale detection is non-trivial
    def = "powerline"
    val = unpackValue <$> MapL.lookup "default_top_theme" cfg


-- colors.json
type ColourDict = Map String Colour
type GradientDict = Map String ColourGradient

data ColourConfig = ColourConfig {
    colourDict   :: ColourDict,
    gradientDict :: GradientDict
} deriving (Generic, Show)

instance FromJSON ColourConfig where
    parseJSON (Object obj) = ColourConfig <$> obj .: "colors"
                                          <*> obj .: "gradients"
    parseJSON invalid = typeMismatch "ColourConfig" invalid

-- Colours can be represented as the cterm code with an optional 24-bit hex representation. e.g. [1, "#AABBCC"]
data Colour = CtermColour Word8
            | TrueColour Word8 Text
            deriving (Generic, Show)

instance FromJSON Colour where
    parseJSON x@(Number _) = CtermColour <$> parseJSON x where
    parseJSON (Array arr) =
        case toList arr of
            [Number c, String h] -> TrueColour <$> parseJSON (Number c) <*> pure h
            _                    -> typeMismatch "Colour" (Array arr)
    parseJSON invalid = typeMismatch "Colour" invalid

-- We use two lists instead of one so that more colours can be used for the true colour gradient
data ColourGradient = CtermGradient [Word8]
                    | TrueGradient [Word8] [Text]
                    deriving (Generic, Show)

instance FromJSON ColourGradient where
    parseJSON (Array arr) =
        case toList arr of
             [cs]     -> CtermGradient <$> parseJSON cs
             [cs, ts] -> TrueGradient  <$> parseJSON cs <*> parseJSON ts
             _        -> typeMismatch "ColourGradient" (Array arr)
    parseJSON invalid = typeMismatch "ColourGradient" invalid


-- colorschemes/*.json

type ColourScheme = Map String TerminalColour

data ColourSchemeConfig = ColourSchemeConfig {
    groups :: ColourScheme,
    modeTranslations :: Map String ColourSchemeConfig
} deriving (Generic, Show)

instance FromJSON ColourSchemeConfig where
    parseJSON (Object obj) = ColourSchemeConfig <$> (derefColourScheme <$> obj .:  "groups")
                                                <*> obj .:? "mode_translations" .!= MapL.empty
    parseJSON invalid      = typeMismatch "ColourSchemeConfig" invalid

-- The JSON representation can contain either a TerminalColour or the key of another colour scheme
-- This is used implicitly in ColourSchemeConfig' FromJSON instance
data TerminalColourEntry = TerminalColourLiteral TerminalColour
                         | TerminalColourRef String
                         deriving (Generic, Show, Eq)
instance FromJSON TerminalColourEntry where
    parseJSON v@(Object _) = TerminalColourLiteral <$> parseJSON v
    parseJSON (String s)   = return . TerminalColourRef $ unpack s
    parseJSON invalid      = typeMismatch "TerminalColourEntry" invalid

-- Despite the name, fg and bg can refer to either simple colours or gradients.
-- DefaultTerminalColour indicates that no highlighting should be performed, and is used for the final divider.
data TerminalColour = TerminalColour {
    fg :: String,
    bg :: String,
    attrs :: [String]
    } | DefaultTerminalColour
    deriving (Generic, Show, Eq)

instance FromJSON TerminalColour where
    parseJSON (Object obj) = TerminalColour <$> obj .: "fg"
                                            <*> obj .: "bg"
                                            <*> obj .: "attrs"
    parseJSON invalid      = typeMismatch "TerminalColour" invalid

-- TODO: this can result in an infinite loop
derefColourScheme :: Map String TerminalColourEntry -> Map String TerminalColour
derefColourScheme origMap = derefCS <$> origMap where  -- maps over values
    derefCS (TerminalColourLiteral cs) = cs
    derefCS (TerminalColourRef k) = derefCS . fromJustNote ("Failed to find colourscheme " ++ k) $ MapL.lookup k origMap

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


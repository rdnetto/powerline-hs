{-# LANGUAGE DeriveGeneric #-}

module ConfigSchema where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Word (Word8)
import Data.Scientific
import Data.Vector as V


-- type alias for entries we don't care about
type DontCare = Object

-- config.json

data MainConfig = MainConfig {
    common :: Map String Value,
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


-- colors.json

data ColourConfig = ColourConfig {
    colors :: Map String Colour,
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
    groups :: ColourScheme
} deriving (Generic, Show)
instance FromJSON ColourSchemeConfig

data TerminalColour = TerminalColour {
    fg :: String,
    bg :: String,
    attrs :: [String]
} deriving (Generic, Show)
instance FromJSON TerminalColour

-- themes/*.json

data ThemeConfig = ThemeConfig {
    default_module  :: Maybe DontCare,
    spaces          :: Maybe Int,
    dividers        :: Maybe (ForBothSides Divider),
    segments        :: ForBothSides [Segment],
    segment_data    :: DontCare
} deriving (Generic, Show)
instance FromJSON ThemeConfig

data ForBothSides a = ForBothSides {
    left    :: a,
    right   :: a
} deriving (Generic, Show)
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

type SegmentArgs = Map String Value


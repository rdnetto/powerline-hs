{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Aeson.Unpack where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as MapS
import Data.Scientific (isInteger, toBoundedInteger, toRealFloat)
import Data.Text (unpack)
import qualified Data.Vector as Vector
import Safe

import Util


class ValueType a where
    unpackValue :: Value -> a

instance ValueType Bool where
    unpackValue (Bool x) = x
    unpackValue x = error $ show x ++ " is not a Bool"

instance ValueType Int where
    unpackValue (Number x) | isInteger x = fromJustNote ("Invalid integer: " ++ show x) $ toBoundedInteger x
    unpackValue x = error $ show x ++ " is not an integer"

instance ValueType Float where
    unpackValue (Number x) = toRealFloat x
    unpackValue x = error $ show x ++ " is not a number"

instance ValueType Rational where
    unpackValue (Number x) = toRational x
    unpackValue x = error $ show x ++ " is not a number"

instance {-# OVERLAPPING #-} ValueType String where
    unpackValue (String x) = unpack x
    unpackValue x = error $ show x ++ " is not a String"

instance ValueType a => ValueType [a] where
    unpackValue (Array xs) = unpackValue <$> Vector.toList xs
    unpackValue x = error $ show x ++ " is not an Array"

instance ValueType v => ValueType (MapS.Map String v) where
    unpackValue (Object hmap) = MapS.fromList $ bimap K.toString unpackValue <$> KM.toList hmap
    unpackValue x = error $ show x ++ " is not an Object"


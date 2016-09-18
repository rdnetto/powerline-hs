{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Aeson_Unpack where

import Data.Aeson (Value(..))
import Data.Maybe (fromJust)
import Data.Scientific (isInteger, toBoundedInteger, toRealFloat)
import Data.Text (unpack)
import Data.Vector (toList)


class ValueType a where
    unpackValue :: ValueType a => Value -> a

instance ValueType Bool where
    unpackValue (Bool x) = x
    unpackValue x = error $ show x ++ " is not a Bool"

instance ValueType Int where
    unpackValue (Number x) | isInteger x = fromJust $ toBoundedInteger x
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
    unpackValue (Array xs) = unpackValue <$> toList xs
    unpackValue x = error $ show x ++ " is not an Array"


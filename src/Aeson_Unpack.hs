{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Aeson_Unpack where

import Data.Aeson (Value(..))
import Data.Maybe (fromJust)
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Text (unpack)


class ValueType a where
    unpackValue :: ValueType a => Value -> a

instance ValueType Bool where
    unpackValue (Bool x) = x
    unpackValue x = error $ show x ++ " is not a Bool"

instance ValueType String where
    unpackValue (String x) = unpack x
    unpackValue x = error $ show x ++ " is not a String"

instance ValueType Int where
    unpackValue (Number x) | isInteger x = fromJust $ toBoundedInteger x
    unpackValue x = error $ show x ++ " is not an integer"


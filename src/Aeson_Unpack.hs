{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Aeson_Unpack where

import Data.Aeson (Value(..))
import Data.Text (unpack)

class ValueType a where
    unpackValue :: ValueType a => Value -> a

instance ValueType Bool where
    unpackValue (Bool x) = x
    unpackValue x = error $ show x ++ " is not a Bool"

instance ValueType String where
    unpackValue (String x) = unpack x
    unpackValue x = error $ show x ++ " is not a String"


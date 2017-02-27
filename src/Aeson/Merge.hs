module Aeson.Merge where

import Data.Aeson.Types
import Data.HashMap.Strict as HMS
import Data.Text (unpack)

-- Recursively merges together two JSON Objects. Right-biased; latter elements override earlier ones.
-- Copied from travis-meta-yaml/Data.Aeson.Merge, because it has ridiculous dependencies for what it does.
mergeJson :: Value -> Value -> Value
mergeJson (Object a) (Object b) = Object (HMS.unionWith mergeJson a b)
mergeJson _ b = b


-- Matches Value.String - convenient for function composition.
string :: Value -> String
string (String s) = unpack s
string x = error $ show x ++ " is not a String"


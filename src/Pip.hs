{-# LANGUAGE TemplateHaskell #-}

module Pip (embedPipVersion) where

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, catMaybes)
import Language.Haskell.TH.Syntax (runIO, Q, Exp)

import Util


-- Finds the installed version of a Pip package
embedPipVersion :: String -> Q Exp
embedPipVersion pkg = do
    v <- runIO (pipVersion pkg)
    case v of
         Just v' -> [| v' |]
         Nothing -> error $ "Could not find pip package " ++ pkg

-- Returns the installed version of a pip package, if present.
pipVersion :: String -> IO (Maybe String)
pipVersion pkg = runMaybeT $ do
    output <- MaybeT $ readProcess "pip" ["show", pkg]
    MaybeT . return . listToMaybe . catMaybes . map (stripPrefix "Version: ") $ lines output


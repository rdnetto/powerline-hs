module Util where

import Control.Monad (liftM)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


-- Wrapper function which simplifies error handling.
readProcess :: FilePath -> [String] -> IO (Maybe String)
readProcess cmd args = do
    (code, stdout, stderr) <- readProcessWithExitCode cmd args ""
    return $ case code of
        ExitSuccess   -> Just $ rtrim stdout
        ExitFailure _ -> Nothing

rtrim :: String -> String
rtrim = dropWhileEnd isSpace

liftM2 :: (Monad m1, Monad m2) => (a -> b) -> m1 (m2 a) -> m1 (m2 b)
liftM2 f = liftM $ liftM f


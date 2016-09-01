module Util where

import Control.Monad (liftM)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)


-- Wrapper function which simplifies error handling.
readProcess :: FilePath -> [String] -> IO (Maybe String)
readProcess cmd args = do
    (code, stdout, _) <- readProcessWithExitCode cmd args ""
    return $ case code of
        ExitSuccess   -> Just $ rtrim stdout
        ExitFailure _ -> Nothing

rtrim :: String -> String
rtrim = dropWhileEnd isSpace

liftM2 :: (Monad m1, Monad m2) => (a -> b) -> m1 (m2 a) -> m1 (m2 b)
liftM2 f = liftM $ liftM f

-- Inserts an element between each adjacent pair of elements, whch is the result of applying a function to those elements.
intersperseBy :: (a -> a -> a) -> [a] -> [a]
intersperseBy f (a0:a1:as) = a0 : f a0 a1 : (intersperseBy f $ a1:as)
intersperseBy _ [x] = [x]
intersperseBy _ [] = []

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

-- Applies a function to the first element of a list only.
mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = []
mapFirst f (x0:xs) = (f x0):xs

-- Applies a function to the last element of a list only.
-- TODO: this could be improved to do a single pass instead of two.
mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f xs = xs' ++ [f xn] where
    xs' = init xs
    xn = last xs

-- Convenient ADT for defining behaviour in terms of which side of the screen we're rendering to
data Side = SLeft | SRight

side :: a -> a -> Side -> a
side l _ SLeft = l
side _ r SRight = r

oppositeSide :: Side -> Side
oppositeSide = side SRight SLeft

-- Helper infix function for specifying a default. The second argument should always be Just.
withDef :: Maybe a -> Maybe a -> a
withDef (Just x) _ = x
withDef _ (Just x) = x
withDef Nothing Nothing = error "withDef called with two Nothings"


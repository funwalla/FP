-- Functors, Applicative Functors and Monoids
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#functors-redux

import Data.Char
import Data.List

main = do line <- fmap ( intersperse '-' . reverse . map toUpper) getLine
          putStrLn line

-- digression: Lifting a function.
-- from: http://www.haskell.org/haskellwiki/Lifting

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

lift :: (a -> b) -> Pair a -> Pair b
lift = fmap

plus2 :: Pair Int -> Pair Int
plus2 = lift (+2)

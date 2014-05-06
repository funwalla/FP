
-- Functors, Applicatives, and Monads in Pictures
-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

import Control.Applicative

half x = if even x
            then Just (x `div` 2)
            else Nothing

data Car = Car {company :: String,
                model :: String,
                year :: Int} deriving (Show)

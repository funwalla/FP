{- Exercise 4.1
Using library functions, define a function
halve :: [a] -> ([a], [a])
that splits an even-lengthed list into two halves.
-}

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

{- Exercise 4.2
Consider a function safetail :: [a] -> [a] that behaves as
the library function tail, except that safetail maps the empty
list to itself whereas tail produces an error in this case.
Define safetail using:
  (a) a conditional expression
  (b) guarded equations
  (c) pattern matching

Hing: make use of the library function null
-}
-- (a) conditional expression
--safetail xs = if null xs then xs else tail xs

-- (b) guarded equations
safetail xs | null xs = xs
            | otherwise tail xs

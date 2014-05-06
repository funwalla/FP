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

-}
safetail :: [a] -> [a]

-- (a) conditional expression
--safetail xs = if null xs then [] else tail xs

-- (b) guarded equations
-- safetail xs | null xs   = xs
--             | otherwise = tail xs

-- (c) pattern matching
safetail [] = []
safetail xs = tail xs

{- Exercise 4.3
In a similar way to ^, show how the logical disjunction
operator v can be defined in four different ways using
pattern matching.
-}

(.|.) :: Bool -> Bool-> Bool
-- first implementation
-- True  .|. True  = True
-- True  .|. False = True
-- False .|. True  = True
-- False .|. False = False

-- second implementation
-- False .|. False = False
-- _ .|.  _ = True

-- third implementation
-- True .|. _ = True
-- False .|. b = b

-- fourth implementation
b .|. c | b == c    =  b
        | otherwise = True

{- Exercise 4.4
Redefine the following version of the conjunction operator
using conditional expressions rather than pattern matching:

True  and b = b
False and _ = False
-}

(.^.) :: Bool -> Bool -> Bool
a .^. b = if a then b
          else False

{- Exercise 4.5
Do the same for the following version, and note the difference
in the number of conditional expressions required:

True ^ True = True
_ ^ _       = False
-}

{- Exercise 4.6
Show how the curried function definition
mult x y z = x * y *z
can be understood in terms of lambda expressions.
-}

mult = \x -> (\y -> (\z -> x * y * z))


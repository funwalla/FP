import Data.Char

-- Chapter 2  Basic Data Types

{- § 2.1.5 Example: computing square roots

Mathematical spec: sqrt ≥ 0 and (sqrt x)^2 = x whenever x ≥ 0.

Weakening the spec to allow for finite precision numbers:
sqrt ≥ 0 and abs((sqrt x)^2 - x) < eps for a suitably small number
eps > 0 whenever x ≥ 0.

Using Newton's method:
If Yn is an approximation to √X then Yn+1 = (Yn + X/Yn)/2
is a better approximation.
-}

-- generate new approximation:
improve     :: Fractional a => a -> a -> a
improve x y =  (y + x/y)/2

-- -- termination test
-- satis x y = abs (y^2 - x) < 0.0001

-- -- repeated application of function f:

until'       :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x |  p x       = x
             |  otherwise = until p f (f x)

-- sqrt' x = until' (satis x) (improve x) x


sqrt' x = until' satis improve x
          where satis y = abs (y^2 - x) < 0.0001
                improve y = (y + x/y)/2

-- more general Newton's method:

deriv f x = ( f (x + dx) - f x)/dx
            where dx = 0.0001

newton f = until' satis improve
           where satis y   = abs(f y) < eps
                 improve y = y - (f y / (deriv f y))
                 eps       = 0.0001

sqrt'' x = newton f x
           where f y = y^2 - x

{- Exercise 2.1.1

     3 `div` 1 * 3 = (3 `div` 1) * 3 = 3 * 3 = 9

     3 * 7 `div` 4 = (3 * 7) `div` 4 = 21 `div` 4 = 5

     6 `div` 2 * 8 `div` 4 = ((6 `div` 2) * 8) `div` 4
                           = (3 * 8) `div` 4 = 24 `div` 4 = 6
-}

{- Exercise 2.1.2  
    x = (x `div` y) * y + (x `mod` y) where 0 ≤ r < y

    Show (x + y) mod z = (x mod z + y mod z) mod z

(x + y) mod z =
    ( [( x div z) * z + (x mod z)] + [( y div z) * z + (y mod z)] ) mod z

Assuming I can distribute the modulo function over the additions above
(big assumption)

= ( x div z) * z mod z + (x mod z) mod z +
  ( y div z) * z mod z + (y mod z) mod z

Further assuming that I can commute the multiplication by z with mod z above:

= ( x div z) mod z * z + (x mod z) mod z +
  ( y div z) mod z * z + (y mod z) mod z

But by definition  ( x div z) mod z =  ( y div z) mod z = 0, so

(x + y) mod z = (x mod z) mod z + (y mod z) mod z
              = (x mod z + y mod z) mod z
-}

{- Exercise 2.1.4  What is the function (+(-x))?

The section (+(-x)) adds (minus x) to its argument
e.g., (+(-x)) y = (+) (-x) y = -x + y = y -x.
-}

f214 = (+ (-6))      -- example with x = 6

y = f214 4      -- will return -2

{- Exercise 2.1.5 for what arguments do the following functions return True?
-}

f215a = (== 9) . (2+) . (7*)

-- f215a will return true for any x such that 7 * x + 2 = 9, i.e., x = 1

f215b = (3>).(mod 2)

-- Since x mod 2 ∈ {0,1} for all positive integers, f215b will be true for all ∈ Z+.

space ::  Int -> String
space n = take n (repeat ' ')

ljustify             :: Int -> String -> String
ljustify n s | n > m =  s ++ space (n - m)
                        where m = length s
rjustify             :: Int -> String -> String                              
rjustify n s | n > m =  space (n - m) ++ s
                        where m = length s
cjustify             :: Int -> String -> String
cjustify n s | n > m =  space lm ++ s ++ space rm
                        where m  = length s
                              lm = (n - m) `div` 2
                              rm = (n - m) - lm

-- Exercise 2.3.1

nextlet   :: Char -> Char
nextlet c =  chr (ord c + 1)  -- no bounds checking implemented

-- Exercise 2.3.2

digitval   :: Char -> Int    -- again, no bounds checking implemented
digitval c =  ord c - ord '0'

{- Exercise 2.3.3

"MacMillan", "Macmillan", "McMillan"

-}

{- Exercise 2.3.4

show (show 42)        value: "\"42\""
show 42 ++ show 42    value: "4242"
show "\n"             value:  "\"\\n\""
                      N.b., GHCi prints the string with all special
                      characters escaped rather than showing the actual
                      newline.
-}

-- Exercise 2.3.4     Doing ljustify only:

ljustify'                 :: Int -> String -> String
ljustify' n s | n > m     =  s ++ space (n - m)
              | otherwise =  s
                             where m = length s

-- § 2.4.1  Example: rational arithmetic

-- define some helper functions

divisors   :: Integral t => t -> [t]                                 
divisors n =  [d | d <- [1 .. n], n `mod` d == 0]

gcd'     :: Integral t => t -> t -> t
gcd' n m =  maximum [d | d <- divisors n, m `mod` d == 0]

-- N.b., gcd is defined in the standard Prelude

norm        :: Integral t => (t, t) -> (t, t)
norm (a, b) =  (u `div` g, v `div` g)
               where u = (signum b) * a
                     v = abs b
                     g = gcd (abs u) v

radd (x, y) (u, v) = norm (x * v + u * y, y * v)

-- Exercise 2.4.1

ageInYears :: Integral t => (t, t, t) -> (t, t, t) -> t
ageInYears (bd, bm, by) (cd, cm, cy) | (bm, bd) <= (cm, cd) = cy - by
                                     | otherwise            = cy - by -1
                                                             

dropThree            :: [a] -> [a]
dropThree (_:_:_:xs) =  xs
dropThree _          = []


-- § 2.5 Patterns

-- cond           :: Bool -> a -> a -> a
-- -- cond True  x y =  x
-- -- cond False x y =  y

-- -- Alternate implementation:
-- cond p x y | p == True = x
--            | otherwise = y

-- (∧)       :: Bool -> Bool -> Bool
-- True  ∧ x =  x
-- False ∧ x =  False

-- (∨)       :: Bool -> Bool -> Bool
-- True  ∨ x =  True
-- False ∨ x =  x

permute :: (Num a, Eq a) => a -> a
permute x | x == 0 = 1
          | x == 1 = 2
          | x == 2 = 0

pred'   :: (Num a, Eq a) => a -> a
pred' n =  n - 1

{- Exercise 2.5.1
Define versions of the functions (∧) and (∨) using patterns for the
second argument. Define versions which use patterns for both arguments. 
Draw up a table showing the values of "and" and "or" for each version . 
-}

-- -- matching on the 2nd argument:
-- (∧)       :: Bool -> Bool -> Bool
-- x ∧ True  =  x
-- x ∧ False =  False

-- (∨)       :: Bool -> Bool -> Bool
-- x ∨ True  =  True
-- x ∨ False =  x

-- matching on both arguments:
(∧)   :: Bool -> Bool -> Bool
x ∧ y |  x == True && y == True = True
      |  otherwise              = False

(∨)   :: Bool -> Bool -> Bool
x ∨ y |  x == False && y == False = False
      |  otherwise                = True

andTable = [x ∧ y|x <- [True, False], y <- [True, False]]
orTable  = [x ∨ y|x <- [True, False], y <- [True, False]]
    
-- § 2.6 Functions

(⊕) :: Num a => a -> a -> a
x ⊕ y = 3 * x + y

-- § 2.6.3 Inverse functions

-- f   :: Num a => a -> (a, a)
-- f x =  (signum x, abs x)

-- finv (x, y) = x * y

-- § 2.6.4  Strict and non-strict functions

three   :: Num a => a -> a
three x =  3

cond :: Bool -> a -> a -> a
cond p x y | p         = x
           | otherwise = y

recip' :: (Fractional a, Eq a) => a -> a
recip' x = cond (x == 0) x (1/x)

{- Exercise 2.6.1
Suppose h x y = f (g x y) . Which of the following statements are true?

(a) h =  f · g        True

g :: a -> b -> c, where c is the output type of g
f :: c -> d, where d is the output type of f
h :: a -> b -> d

	(b) h x = f . ( g x)
	(c) h x y = (f · g ) x y
-}

g     :: Num a => a -> a -> a
g x y =  x + y

f   :: (Num a, Show a) => a -> [Char]
f x =  "The sum is " ++ show x

h x y = f (g x y)



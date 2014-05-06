{- Exercise 2.1
Parenthesize the following arithmetic expressions:
2 ^ 3 * 4     = (2 ^ 3) * 4
2 * 3 + 4 * 5 = (2 * 3) + (4 * 5)
2 + 3 * 4 ^ 5 = 2 + (3 * (4 ^ 5))
-}

{- Exercise 2.3
The script below contains 3 syntatic errors. Correct these errors
and then check using Hugs.
N = a 'div' length xs  -- `div` not 'div', function names lowercase
    where
      a  = 10
     xs = [1,2,3,4,5]  -- xs is offsides
-}
n = a `div` length xs
    where
      a  = 10
      xs = [1,2,3,4,5]

{- Exercise 2.4
Show how the library function last that selects the last element
of a non-empty list could be defined in terms of the library
functions introduced in this chapter. Can you think of another
possible definition?
-}
last1 xs = xs !! (length xs - 1)
last2 xs = product (take 1 (reverse xs))
last3 xs = sum (take 1 (reverse xs))
last4 xs = sum (drop (length xs - 1) xs)

-- book gives
last5 xs = head (reverse xs)

{- Exercise 
Show how the library function init that removes the last element
from a non-empty list could similarly be defined in two different
ways.
-}
init1 xs = take (length xs - 1) xs
init2 xs = reverse (tail (reverse xs))

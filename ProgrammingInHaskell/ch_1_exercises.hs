{- Exercise 1.1
Give another possible calculation for the result of double (double 2)

(a) double (double 2) = double (2 + 2) = double 4 = 4 + 4 = 8

(b) double (double 2) = double 2 + double 2 = (2+2) + (2+2) = 4+4 = 8

(c) Defining double x = 2 * x gives
    double (double 2) = double (2 * 2) = double 4 = 2 * 4 = 8 or
    double (double 2) = 2 * (double 2) = 2 * (2 * 2) = 2 * 4 = 8

Note: Hutton uses (a) and (b) and varies the order of evaluation of
the addition.
-}

{- Exercise 1.2
Show that sum [x] = x for any number x

From the definition: sum [] = 0 and sum x:xs = x + sum xs.
Then for any number x: sum [x] = sum [x:[]] = x + sum [] = x + 0 = x
-}

{- Exercise 1.3
Define a function product that produces the product of a list of
numbers and show, using your definition that product [2,3,4] = 24.
-}

prod []     = 1
prod (x:xs) = x * product xs
-- prod [2,3,4] = 24 as required.

{- Exercise 1.4
How should the definition of the function qsort be modified so
that it produces a reverse sorted version of a list?
-}

-- Original definition:
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b >  x]

-- Reverse sort:
revQsort [] = []
revQsort (x:xs) = revQsort larger ++ [x] ++ revQsort smaller
                  where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [b | b <- xs, b >  x]

{- Exercise 1.5
What would be the effect of replacing <= by < in the definition of
qsort? Hint: consider the example qsort [2,2,3,1,1].

The use of <= in the definition of smaller in qsort will cause any
subsequent occurrences x after the first to be placed in the smaller
list. It thus retains multiple occurrences of a number in the result.
If <= is replaced by < then duplicates will be eliminated during the
sort.

qsort [2,2,3,1,1] = [1,1,2,2,3]
-}

setQsort [] = []
setQsort (x:xs) = setQsort smaller ++ [x] ++ setQsort larger
                  where
                    smaller = [a | a <- xs, a < x]
                    larger  = [b | b <- xs, b > x]

-- setQsort [2,2,3,1,1] = [1,2,3]

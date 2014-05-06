
-- 6.1 Basic concepts

factorial   :: Int -> Int
factorial 0 =  1
factorial n =  n * factorial (n-1)

(.*.) :: Int -> Int -> Int
m .*. 0 = 0
m .*. n = m + m .*. (n - 1)

-- 6.2 Recursion on lists

product'        :: Num a => [a] -> a
product' []     =  1
product' (n:ns) =  n * product' ns

length'        :: [a] -> Int
length' []     =  0 
length' (_:xs) =  1 + length' xs

reverse'        :: [a] -> [a]
reverse' []     =  []
reverse' (x:xs) =  reverse' xs ++ [x]

append'           :: [a] -> [a] -> [a]
append' [] ys     = ys
append' (x:xs) ys = x : append' xs ys

insert'                      :: (Ord a) => a -> [a] -> [a]
insert' x []                 = [x]
insert' x (y:ys) | x <= y    = x : y : ys
                 | otherwise = y : insert' x ys

isort    :: (Ord a) => [a] -> [a]
isort []     =  []
isort (x:xs) =  insert' x (isort xs)

-- 6.3 Multiple arguments

drop'               :: Integral b => b -> [a] -> [a]
drop' n xs | n <= 0 =  xs
drop' _ []          =  []
drop' n (_ : xs)    =  drop' (n-1) xs

-- 6.4 Multiple recursion

fibonacci       :: Int -> Int
fibonacci 0     =  0
fibonacci 1     =  1
fibonacci n =  fibonacci (n - 1) + fibonacci (n - 2)

qsort    :: Ord a => [a] -> [a]
qsort []     =  []
qsort (x:xs) =  qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b >  x]

-- 6.5 Mutual recursion

even'   :: Int -> Bool
even' 0 =  True
even' n =  odd' (n-1)

odd'   :: Int -> Bool
odd' 0 =  False
odd' n =  even' (n-1)

evens        :: [a] -> [a]
evens []     =  []
evens (x:xs) =  x : odds xs

odds        :: [a] -> [a]
odds []     =  []
odds (_:xs) =  evens xs

-- 6.6 Advice on recursion

init'          :: [a] -> [a]
init' [x]      =  []
init' (x : xs) =  x : init' xs
init' []       =  error "init: the list cannot be empty."

----------  Exercises  ------------------------------------------

{- Exercise 6.1
Define the exponentiation operator, ^, for non-negative integers
using the same pattern of recursion as the multiplication operator
and show how 2^3 is evaluated using your definition.
-}
(.^.) :: Int -> Int -> Int
m .^. 0 =  1
m .^. n =  m * m .^. (n - 1)

{- Evaluation of 2 .^. 3:
2 .^. 3 = 2 * (2 .^. (3-1)) = 2 * (2 .^. 2)
        = 2 * (2 * (2 .^. (2-1))) = 2 * (2 * (2 .^. 1))
        = 2 * (2 * (2 * (2 .^. 1-1))) = 2 * (2 * (2 * (2 .^. 0)))
        = 2 * (2 * (2 * 1)) = 2 * (2 * 2) = 2 * 4 = 8
-}

{- Exercise 6.2
Using the definitions in the chapter, show how length [1,2,3],
drop [1,2,3], and init [1,2,3] are evaluated.

len           :: [a] -> Int
len []        =  0
len ( _ : xs) = 1 + length xs

len [1,2,3] = 1 + len [2,3] = 1 + (1 + len [3])
            = 1 + (1 + (1 + len [])) = 1 + (1 + (1 + 0))
            = 1 + (1 + 1) = 1 + 2 = 3

mydrop :: Int ->  [a] -> [a]
mydrop 0 xs = xs
mydrop _ [] = []
mydrop n (_ : xs) = mydrop (n-1) xs
-}

{- Exercise 6.3
Without looking at the definitions from the standard prelude,
define the following library functions using recursion.
-}

-- Decide if all logical values in a list are True:
and'          :: [Bool] -> Bool
and' []       =  True
and' (b : bs) =  b && (and bs)

-- Concatenate a list of lists:
concat'    :: [[a]] -> [a]
concat' []         =  []
concat' (xs : xss) =  xs ++ (concat' xss)

-- Produce a list with n identical elements:
replicate'     :: Int -> a -> [a]
replicate' 0 x =  []   -- better: replicate 0 _ = []
replicate' n x =  x : replicate' (n-1) x

-- Select the nth element of a list:
(!!!)          :: [a] -> Int -> a
(x : xs) !!! 0 =  x   -- better: (x : _) !!! 0 = x
(x : xs) !!! n =  xs !!! (n-1)

-- Decide if a value is an element of a list:
elem'                        :: Eq a => a -> [a] -> Bool
elem' y []                   =  False
elem' y (x : xs) | y == x    =  True
                 | otherwise =  elem' y xs

{- Exercise 6.4
Define a recursive function
    merge :: Ord a => [a] -> [a] -> [a]
that merges two sorted lists to give a single sorted list.
Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.
-}
merge                            ::  Ord a  => [a] -> [a] -> [a]
merge xs []                      =  xs
merge [] ys                      =  ys
merge (x:xs) (y:ys) | x <= y     = x : merge xs (y:ys)
                    | otherwise  = y : merge (x:xs) ys
{- Exercise 6.5
Using merge, define a recursive function
    msort :: Ord a => [a] -> [a]
that implements merge sort, in which the empty list and
singleton lists are already sorted, and any other list is
sorted by merging together the two lists that result from
sorting the two halves of the list separately.

Hint: first define a function halve :: [a] -> ([ a], [a]) that
splits a list into two halves whose lengths differ by at most one.
-}

halve    :: [a] -> ([a], [a])
halve xs =  (take midpt xs, drop midpt xs)
            where midpt = length xs `div` 2

msort        :: Ord a => [a] -> [a]
msort []     =  []
msort [x] =  [x]
msort xs  =  merge (msort ys) (msort zs)
             where (ys, zs) = halve xs

{- Exercise 6.6
Using the five-step process, define the library functions that
calculate the sum of a list of numbers, take a given number of
elements from the start of a list, and select the last element
of a non-empty list.

Step 1. Define the type
Step 2. Enumerate the cases
Step 3. Define the simple cases
Step 4. Define the other cases
Step 5. Generalize and simplify
-}

-- sum of a list of numbers
sum' :: Num a => [a] -> a -- 1. Define type
sum' [] = 0                 -- 2. Enumerate cases, 3. Define simple cases
sum' (x:xs) = x + sum' xs   -- 4. Define other cases
-- 5. Generalize and simplify
-- Hutton starts with [Int] in step 1 and generalizes to Num a here.
-- He also simplifies the definition using foldr
betterSum :: Num a => [a] -> a
betterSum = foldr (+) 0

-- take a given number of elements from the start of a list
-- Step 1. Define the type
take' :: Int -> [a] -> [a]
-- Step 2. Enumerate the cases
-- Step 3. Define the simple cases
-- take' 0 [] = []
-- take' 0 xs = []
-- take' n [] = []
-- -- Step 4. Define the other cases
-- take' n (x:xs) = x : take' (n-1) xs
-- Step 5. Generalize and simplify
take' 0 _ = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

-- select the last element of a non-empty list.

-- Step 1. Define the type
last' :: [a] -> a
-- Step 2. Enumerate the cases
-- Step 3. Define the simple cases
last' [x] = x
-- Step 4. Define the other cases
last' (_:xs) = last' xs
-- Step 5. Generalize and simplify

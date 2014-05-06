{- Reading notes:

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <-  xs]

first' :: [(a,b)] -> [a]
--first' xs = [fst x | x <- xs]
first' xs = [x | (x,_) <- xs]

len' :: [a] -> Int
len' xs = sum [1 | _ <- xs]

factors' :: Integral t => t -> [t]
factors' n = [x | x <- [1 .. n], n `mod` x == 0]

prime' :: Int -> Bool
--prime' n = len' (factors' n) == 2
-- the book's version is clearer and faster since any factor
-- other than 1 or n will stop the evaluation of factors' n.
prime' n = factors' n == [1,n]

allPrimes :: Int -> [Int]
allPrimes n = [m | m <- [1 .. n], prime' m]

find' :: Eq a => a -> [(a,b)] -> [b]
find' x xprs = [b | (a,b) <- xprs, a == x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

pos' :: (Enum t, Eq a, Num t) => a -> [a] -> [t]
pos' z zs = [y | (x,y) <- zip zs [0 ..], x == z]

numLowerCase :: [Char] -> Int
numLowerCase cs = length [c | c <- cs, c `elem` ['a' .. 'z']]

countChar :: Char -> [Char] -> Int
countChar c cs = length [c' | c' <- cs, c' == c]

End Reading Notes -}

{- Exercise 5.1
Using a list comprehension, give an expression that calculates the
sum of the first one hundred integer squares. 
-}

sumSquares       :: Int -> Int -> Int
sumSquares n1 n2 =  sum [x^2 | x <- [n1 .. n2]]

{- Exercise 5.2
In a similar way to the function length, show how the library
function replicate :: Int → a → [a] that produces a list of
identical elements can be defined using a list comprehension.
For example:
> replicate 3 True
[True, True, True]
-}

replicate'     :: Int -> a -> [a]
replicate' n x =  [x | _ <- [1 .. n]]

{- Exercise 5.3
A triple (x, y, z) of positive integers is pythagorean if
x2 + y2 = z2. Using a list comprehension, define a function
pyths :: Int → [( Int, Int, Int)] that returns the list of all
pythagorean triples whose components are at most a given limit.
For example:
> pyths 10
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)]
-}

pyths   :: Int -> [( Int, Int, Int)]
pyths n =  [(x, y, z) | x <- [1 .. n],
                        y <- [1 .. n],
                        z <- [1 .. n],
                        x^2 + y^2 == z^2]

{- Exercise 5.4
A positive integer is perfect if it equals the sum of its factors,
excluding the number itself. Using a list comprehension and the
function factors, define a function perfects :: Int → [Int] that
returns the list of all perfect numbers up to a given limit.
For example:
> perfects 500
[6, 28, 496]
-} 

factors   :: Int -> [Int]
factors n =  [m | m <- [1 .. n], n `mod` m == 0]

perfects   :: Int -> [Int]
perfects n =  [m | m <- [1 .. n], sum (factors m) == 2 * m]

{- book's equivalent solution:
perfects n =  [m | m <- [1 .. n], sum (init (factors m)) == m]
-}

{- Exercise 5.5
Show how the single comprehension
[( x, y) | x ← [1, 2, 3], y ← [4, 5, 6]] with two generators can
be re-expressed using two comprehensions with single generators.
Hint: make use of the library function concat and nest one
comprehension within the other. 
-}

cartProd       :: [a] -> [b] -> [(a,b)]
cartProd xs ys =  concat [[ (x,y) | y <- ys] | x <- xs]

{- Exercise 5.6
Redefine the function positions using the function find.
-}

find :: Eq a => a -> [(a,b)] -> [b]
find x xprs = [b | (a,b) <- xprs, a == x]

positions2      :: (Enum t, Eq a, Num t) => a -> [a] -> [t]
positions2 z zs =  find z (zip zs [0 ..])

{- Exercise 5.7
The scalar product of two lists of integers xs and ys of length n
is given by the sum of the products of corresponding integers:
Sum from i=0 to n-1: (xsi * ysi)

In a similar manner to the function chisqr, show how a list
comprehension can be used to define a function
scalarproduct :: [Int] → [Int] → Int that returns the scalar
product of two lists.
For example: 
> scalarproduct [1,2,3] [4,5,6]
32
-}

scalarproduct       :: [Int] -> [Int] -> Int
scalarproduct xs ys =  sum [x * y | (x, y) <- zip xs ys]

{- Exercise 5.8
Modify the Caesar cipher program to also handle upper-case letters.

See ch_5_caesar_cipher.hs
-}



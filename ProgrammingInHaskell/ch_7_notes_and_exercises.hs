-- Chapter 7 Higer-order functions

import Data.Char

-- 7.1 Basic concepts

twice     :: (a -> a) -> a -> a
-- twice f x =  f (f x)
twice f = f . f             -- using function composition

quad = twice (* 2)

-- 7.2 Processing lists

-- map'      :: (a -> b) -> [a] -> [b]
-- -- Using a list comprehension:
-- -- map' f xs =  [f x | x <- xs]
-- -- Using recursion:
-- map' f []        = []
-- map' f (x : xs) = f x : map' f xs

-- filter' :: (a -> Bool) -> [a] -> [a]
-- -- Using a list comprehension:
-- -- filter' p xs = [x | x <- xs, p x]
-- -- Using recursion:
-- filter' p []     = []
-- filter' p (x : xs) | p x       = x : filter' p xs
--                    | otherwise = filter' p xs

-- all :: (a -> Bool) [a] -> Bool
-- any :: (a -> Bool) [a] -> Bool
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile :: (a -> Bool) -> [a] -> [a]

-- 7.3 The foldr function

foldr'                :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc []       =  acc
foldr' f acc (x : xs) =  f x (foldr' f acc xs)

--   foldr f a [x0, x1, x2]
-- = f x0 (foldr f a [x1,x2])
-- = f x0 (f x1 (foldr f a [x2]))
-- = f x0 (f x1 (f x2 (foldr f a [])))
-- = f x0 (f x1 (f x2 a))
--   e.g., if f = (+): x0 + (x1 + (x2 + a))

{- example: product = foldr (*) 1

product [1,2,3]
= foldr (*) 1 [1,2,3]
= 1 * (foldr (*) 1 [2,3]) -- using infix outside of foldr'
= 1 * (2 * (foldr (*) 1 [3]))
= 1 * (2 * (3 * (foldr (*) 1 [])))
= 1 * (2 * (3 * 1))

foldr can be viewed as replacing the empty list with the
accumulator and the cons operator with the function starting from
the right:
foldr f a (x : (y : (z : [])))
= f x (f y (f z a))
-}

-- length' = foldr (\_ n -> 1 + n) 0

-- reverse' = foldr (\x xs -> xs ++ [x]) []

-- 7.4 The foldl function

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc [] = acc 
foldl' f acc (x:xs) = foldl' f (f acc x) xs

{- foldl' f a [x0,x1,x2]
=  foldl' f (f a x0) [x1,x2]
=  foldl' f (f (f a x0) x1) [x2]
=  foldl' f (f (f (f a x0) x1) x2) []
=  f (f (f a x0) x1) x2
-}

--product, or, length, reverse (++)

product' :: Num a => [a] -> a
product' =  foldl (*) 1

or' :: [Bool] -> Bool
or' = foldl (||) False

--   foldl (||) F [T,F]
-- = foldl (||) (F || T) [F]         -- by Clause 1
-- = foldl (||) ((F || T) || F) []   -- by Clause 1
-- = ((F || T) || F)                 -- by Clause 2 w/ acc = ((F || T) || F)
-- = (T || F) = T

length' :: [a] -> Int
length' = foldl (\n _ -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' = foldl (\ acc x -> x : acc) []

--   foldl (\ a x -> x:a) []                    [1,2,3]
-- = foldl (\ a x -> x:a) (1 : [])             [2,3]
-- = foldl (\ a x -> x:a) (2 : (1 : []))       [3]
-- = foldl (\ a x -> x:a) (3 : (2 : (1 : []))) []
-- =                      (3 : (2 : (1 : [])))

{-
example: product = foldl (*) 1

product [1,2,3]
= foldl (*) 1 [1,2,3]
= foldl (*) (1 * 1) [2,3]   -- using infix form in the accumulator
= foldl (*) ((1 * 1) * 2) [3]
= foldl (*) (((1 * 1) * 2) * 3) []
= ((1 * 1) * 2) * 3

foldl f a [x,y,z]
= f (f (f a 1) 2) 3
-}

elem' e xs = foldl (\acc x -> acc || x == e) False xs

-- 7.5 The composition operator

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g =  \x -> f(g x)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g =  \x -> f(g x)

compose     :: (b -> c) -> (a -> b) -> a -> c
compose f g =  \x -> f(g x)

odd1 n = not (even n)
odd2   = not . even
odd3   = compose not even

sumSqrEvens1 ns = sum (map (^) (filter even ns))
--sumSqrEvens2    = sum . map (^) . filter even

(.++.) = foldr (:)

-- 7.6 String transmitter

type Bit = Int

bin2int :: [Bit] -> Int
--bin2int bits = sum [w * b | (w,b) <- zip weights bits]
--               where weights = iterate (* 2) 1

{-      simpler definition of bin2int

For an arbitrary 4-bit binary number [a,b,c,d]
bin2int [a,b,c,d]
= (1 * a) + (2 * b) + (4 * c) + (8 * d)
=    a    + (2 * b) + (4 * c) + (8 * d)  -- simplifying 1 * a
=    a    + 2 * (b + 2 * c + 4 * d)      -- factoring out 2
=    a    + 2 * (b + 2 * (c + 2 d))      -- factoring out 2
=    a    + 2 * (b + 2 * (c + 2 * (d + 2 * (0))))

i.e., to convert [a,b,c,d] = a : (b : (c : (d : ([])))
replace each cons by (+) . (2 *) and replace [] with 0
-}                    

-- bin2int = foldr ((+) . (2 *)) 0  -- wrong, find out why

{- example bin2int [1,0,1]
foldr ((+) . (2 *)) 0 [1,0,1] -- use f = ((+) . (2 *))
= f 1 (foldr f 0 [0,1])
= f 1 ( f 0 (foldr f 0 [1]))
= f 1 ( f 0 (f 1 (foldr f 0 [])))
= f 1 ( f 0 (f 1 0))
= f 1 ( f 0 2)
= f 1 ( 2)
= 4
The composed function badf = (+) . (2 *) multiplies its
first argument by 2 then adds the result to its second
argument. reversing the order of the arguments fixes this.
try reversing the composition

-}

--badf = (*2 ).(+)


bin2int = foldr (\x y -> x + 2 * y) 0

{- example bin2int [1,0,1]

foldr (\x y -> x + 2 * y) 0 [1,0,1]
= 1 + 2 * (foldr \ 0 [0,1])   -- using \ for the full lambda function
= 1 + 2 * ( 0 + 2 * (foldr \ 0 [1]))
= 1 + 2 * ( 0 + 2 * (1 + 2 * (foldr \ 0 [])))
= 1 + 2 * ( 0 + 2 * (1 + 2 * (0)))
= 1 + 2 * ( 0 + 2 * (1))
= 1 + 2 * ( 2 ) = 5
-}

{- int2bin calculation
13/2 = 6 remainder 1
 6/2 = 3 remainder 0
 3/2 = 1 remainder 1
 1/2 = 0 remainder 1
-}

int2bin   :: Int -> [Bit]
int2bin 0 =  []
int2bin n =  n `mod` 2 : int2bin (n `div` 2)

make8      :: [Bit] -> [Bit]
make8 bits =  take 8 (bits ++ repeat 0)

-- Transmission

encode :: String -> [Bit]
encode =  concat . map (make8 . int2bin . ord)

chop8    :: [Bit] -> [[Bit]]
chop8 []   =  []
chop8 bits =  take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel =  id

transmit :: String -> String
transmit =  decode . channel . encode

-----   Exercises

{- Exercise 7.1
Show how the list comprehension [f x | x <- xs, p x] can
be re-expressed using the higher-order functions map and filter.

(map f . (filter p)) xs   -- works
--map f (filter p xs)       -- book solution
-}

{- Exercise 7.2
Define the higher-order functions all, any, takeWhile, & dropWhile.
-}

all'   :: (a -> Bool) -> [a] -> Bool
all' p =  and . map p

any'   :: (a -> Bool) -> [a] -> Bool
any' p =  or . map p

takeWhile'                        :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                   = []
takeWhile' p (x : xs) | p x       = x : takeWhile' p xs
                      | otherwise = []

dropWhile'                        :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                   = []
dropWhile' p (x : xs) | p x       = dropWhile' p xs
                      | otherwise = []

{- Exercise 7.3
Redefine the functions map f and filter p using foldr.

For map: apply a function to each list element and then
cons it to the next
-}

--map' :: (a -> b) -> [a] -> [b]
map' f =  foldr (\x xs -> f x : xs) []

filter' p =  foldr (\x xs -> if (p x) then x : xs else xs) []

{- filter' (< 3) [1,2,3]
foldr (\x xs  -> if (x < 2) then x:xs else xs) []
f 1 (f 2 (f 3 (f[]))
f 1 (f 2 (f 3 ([]))
f 1 (f 2 [])
f 1 (2:[])
1:2:[]
-}

{- Exercise 7.4
Using foldl, define a function dec2int :: [Int] -> Int
that converts a decimal number into an integer
e.g., dec2int [2,3,4,5] = 2345
-}

dec2int :: [Int] -> Int
dec2int =  foldl (\ x y -> 10 * x + y) 0

{- Exercise 7.5
Explain why the following definition is invalid

sumsqreven = compose [sum, map(^2), filter even]

compose     :: (b -> c) -> (a -> b) -> a -> c
compose f g =  \x -> f(g x)

The simple answer is that the compose function as defined
above does not take a list argument. The book points out that
the functions are not all of the same type and thus cannot be
members of the same list.

The following use of compose would work:
-}
sumsqreven' = compose sum (compose (map(^2)) (filter even))

{- Exercise 7.6
Define the higher-order library function curry that converts
a function on pairs into a curried function and the function
uncurry that converts a curried function with two arguments
into a function on pairs.

Hint: first write down the types of the two functions
-}

curry' :: ((a,b) -> c) -> (a -> b -> c)
-- curry' f a b = f (a, b)      -- my implementation
curry' f = \ x y -> f (x, y)    -- book's implementation is cleaner

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
-- uncurry' f (a, b) = f a b    -- my implementation
uncurry' f = \ (x, y) -> f x y  -- book's implementation is cleaner

f (a,b) = a + b
fc = curry' f

g a b = a * b
guc = uncurry' g

{- Exercise 7.7
A higher-order function unfold that encapsulates a simple pattern
of recursion for producing a list can be defined as follows:     -}

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)
{-
That is,the function unfold p h t produces the empty list if the
predicate p is true of the argument, and otherwise produces a
non-empty list by applying the function h to give the head, and the
function t to generate another argument that is recursively processed
in the same way to produce the tail of the list.

For example, the function int2bin

int2bin   :: Int -> [Bit]
int2bin 0 =  []
int2bin n =  n `mod` 2 : int2bin (n `div` 2)

can be rewritten more compactly using unfold as follows:  -}

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

{- Redefine the functions chop8, map f, and iterate f using unfold.

chop8    :: [Bit] -> [[Bit]]
chop8 []   =  []
chop8 bits =  take 8 bits : chop8 (drop 8 bits)            -}

chop8' = unfold null (take 8) (drop 8)

mapuf f = unfold null (f . head) tail

iterate' f = unfold (\x -> False) id f
-- note: book uses (const False) rather than the lambda function



multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

applyTwice     :: (a -> a) -> a -> a
applyTwice f x =  (f . f) x

zipWith'                 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          =  []
zipWith' _ _ []          =  []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

flip'   :: (a -> b -> c) -> (b -> a -> c)
-- flip' f =  g
--            where g x y = f y x
flip' f y x = f x y             -- better implementation

-- map'            :: (a -> b) -> [a] -> [b]
-- map' _ []       =  []
-- map' f (x : xs) = f x : map' f xs

filter'                        :: (a -> Bool) -> [a] -> [a]
filter' _ []                   =  []
filter' p (x : xs) | p x       =  x : filter' p xs
                   | otherwise =  filter' p xs

qs          :: (Ord a) => [a] -> [a]
qs []       =  []
qs (x : xs) =  smallerXs ++ [x] ++ largerXs
               where smallerXs = qs (filter' (< x) xs)
                     largerXs  = qs (filter' (>= x) xs)

largestDivisible :: (Integral a) => a -> a -> a
largestDivisible max divisor = head (filter p [max, (max - 1) ..])
                                 where p x = x `mod` divisor == 0

-- sum (takeWhile (< 10000) (filter odd (map (^2) [1 ..])))

chain            :: (Integral a) => a -> [a]
chain 1          =  [1]
chain n | even n =  n : chain (n `div` 2)
        | odd n  =  n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains =  length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

-- Only folds and horses
{-
foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn

foldl              :: (a -> b -> a) -> a -> [b] -> a
foldl f acc []     =  acc 
foldl f acc (x:xs) =  foldl f (f acc x) xs
-}

mapl      :: (a -> b) -> [a] -> [b]
mapl f xs =  foldl (\ acc x -> acc ++ [f x]) [] xs

sum'    :: (Num a) => [a] -> a
sum' xs =  foldl (\acc x -> acc - x) 0 xs
                       
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

{-
-- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

foldr                :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []       =  acc
foldr f acc (x : xs) =  f x (foldr f acc xs)
-}

map'      :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

{-  Example

foldr (\x acc -> f x : acc) [] [x1,x2,x3]
= f x1 : (foldr f [] [x2, x3])
= f x1 : (f x2 : (foldr f [] [x3]))
= f x1 : (f x2 : (f x3 : (foldr f [] [])))
= f x1 : (f x2 : (f x3 : []))
-}

-- maximuml    :: (Ord a, Num a) => [a] -> a
-- maximuml xs =  foldl (\ acc x -> if acc < x then x else acc) 0 xs

-- maximumr    :: (Ord a, Num a) => [a] -> a
-- maximumr xs =  foldr (\x acc -> if x > acc then x else acc) 0 xs

-- Better:
maximuml :: (Ord a, Num a) => [a] -> a
maximuml =  foldl1 (\ acc x -> if acc < x then x else acc)

maximumr :: (Ord a, Num a) => [a] -> a
maximumr =  foldr1 (\x acc -> if x > acc then x else acc)

reversel :: [a] -> [a]
reversel = foldl (\ acc x -> x : acc) []

reverser    :: [a] -> [a]
reverser =  foldr (\ x acc -> acc ++ [x]) []

productl :: (Num a) => [a] -> a
productl =  foldl (*) 1

filterl               :: (a -> Bool) -> [a] -> [a]
filterl p =  foldl (\ acc x -> if p x then acc ++ [x] else acc) []

filterr               :: (a -> Bool) -> [a] -> [a]
filterr p =  foldr (\ x acc -> if p x then x : acc else acc) []

headr :: [a] -> a
headr =  foldr1 (\x _ -> x)

headl :: [a] -> a
headl =  foldl1 (\_ x -> x)

lastr :: [a] -> a
lastr =  foldr1 (\x acc -> acc)

lastl :: [a] -> a
lastl =  foldl1 (\_ x -> x)

--length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- Function application with $

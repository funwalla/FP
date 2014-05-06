maximum'          :: (Ord a) => [a] -> a
maximum' []       = error "maximum of an empty list"
maximum' [x]      = x
maximum' (x : xs) = max x (maximum' xs)

replicate'                 :: (Num i, Ord i) => i -> a -> [a]
replicate' n x | n <= 0    = []
               | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ []                   = []
take' n (x : xs) | n <= 0    = []
                 | otherwise = x : take' (n - 1) xs

reverse'          :: [a] -> [a]
reverse' []       =  []
reverse' (x : xs) =  reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip'                   :: [a] -> [b] -> [(a,b)]
zip' _ []              =  []
zip' [] _              =  []
zip' (x : xs) (y : ys) =  (x, y) : zip' xs ys

elem'                        :: (Eq a) => a -> [a] -> Bool
elem' x []                   =  False
elem' x (y : ys) | x == y    =  True
                 | otherwise =  elem' x ys

qs'          :: (Ord a) => [a] -> [a]
qs' []       =  []
qs' (x : xs) =  smallerSorted ++ [x] ++ largerSorted
                where smallerSorted = qs' [s | s <- xs, s < x]
                      largerSorted  = qs' [l | l <- xs, l >= x]

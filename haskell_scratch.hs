
fl              :: (a -> b -> a) -> a -> [b] -> a
fl f acc []     =  acc
fl f acc (x:xs) =  fl f (f acc x) xs

fr              :: (a -> b -> b) -> b -> [a] -> b
fr f acc []     =  acc
fr f acc (x:xs) =  f x (fr f acc xs)

-- comment added while learning git.
-- one more change.

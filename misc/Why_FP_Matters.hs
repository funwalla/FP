
-- Section 3 Glueing Functions Together

sum' :: Num a => [a] -> a
sum' []       = 0
sum' (x : xs) =  x + sum' xs

sum2 = foldr (+) 0  -- n.b., Hughes uses "reduce" rather than "foldr"

foldr' f x [] = x
foldr' f x (y : ys) = f y (foldr' f x ys)

{- Example
foldr' (+) 0 [1,2,3] = 1 + (foldr' (+) 0 [2,3])
                     = 1 + (2 + (foldr' (+) 0 [3]))
                     = 1 + (2 + (3 + (foldr' (+) 0 [])))
                     = 1 + (2 + (3 + (0)))
-}

product' = foldr' (*) 1
anytrue' = foldr' (||) False
alltrue' = foldr' (&&) True

append' a b = foldr' (:) b a

{- Example
append' [1,2] [3,4] = foldr' (:) [3,4] [1,2]
                    = 1 : (foldr' (:) [3,4] [2])
                    = 1 : (2 : (foldr' (:) [3,4] []))
                    = 1 : (2 : ([3,4]))
-}

--doublecons num lst = (2 * num) : lst
doubleall = foldr' doublecons []

{- Example
doubleall [1,2] = foldr' doublecons [] [1,2]
                = doublecons 1 (foldr' doublecons [] [2])
                = doublecons 1 (doublecons 2 (foldr' doublecons [] []))
                = doublecons 1 (doublecons 2 ([]))
                = doublecons 1 (4 : [])
                = 2 : (4 : [])
-}

double n = 2 * n
fandcons f el lst = (:) (f el) lst

doublecons = fandcons double

map' f = foldr' ((:) . f) []

{- Example
map' (2*) [1,2] = foldr' ((:) . (2*)) [] [1,2]
                = ((:) . (2*)) 1 (foldr' ((:) . (2*)) [] [2])
                = ((:) . (2*)) 1 (((:) . (2*)) 2 (foldr' ((:) . (2*)) [] []))
                = ((:) . (2*)) 1 (((:) . (2*)) 2 ([]))
                = ((:) . (2*)) 1 ((:) 4 [])
                = (:) (2*1) [4]
                = [2,4]
-}

summatrix = sum2 . map' sum2

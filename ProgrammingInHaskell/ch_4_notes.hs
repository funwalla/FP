test :: [Char] -> Bool
test ('a' : _) = True
test _ = False

-- pred :: Int -> Int
-- pred 0       = 0
-- pred (n + 1) = n

add' = \x -> (\y -> x + y)

const' :: a -> (b -> a)
const' x = \ _ -> x

tst = (+2)

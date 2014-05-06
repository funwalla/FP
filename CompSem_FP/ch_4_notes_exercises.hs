band :: Bool -> Bool -> Bool
b `band` c
  | b == c = b
  | otherwise = False

test :: [Char] -> Bool
test ('a' : _) = True
test _         = False

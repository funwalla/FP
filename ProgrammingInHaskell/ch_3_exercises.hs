{- Exercise 3.1
What are the types of the following values?

['a','b','c'] :: [Char]
('a','b','c') :: (Char, Char, Char)
[(False, '0'), (True, '1')] :: [(Bool, Char]
([False, True], ['0', '1']) :: ([Bool], [Char])
[tail, init, reverse] :: [[a] -> [a]]
-}

{- Exercise 3.2
What are the types of the following functions?
-}
second xs = head (tail xs) --  second :: [a] -> a
swap (x,y) = (y,x) -- swap :: (a,b) -> (b,a)
pair x y = (x,y) -- pair :: a -> b -> (a,b)
double x = x * 2 -- double :: Num a => a -> a
palindrome xs = reverse xs == xs -- palindrome :: Eq a => [a] -> Bool
twice f x = f (f x) -- twice :: (a -> a) -> a -> a

{- Exercise 3.4
Why is it not feasible in general for function types to be instances
of the EQ class?
Because there is no general way to determine the equality of two
functions.

When is it feasible?

Book's answer:
In general, checking if two functions are equal requires enumerating
all possible argument values, and checking if the functions give the
same result for each of these values. For functions with a very large
(or inﬁnite) number of argument values, such as values of type Int or
Integer, this is not feasible. However, for small numbers of argument
values, such as values of type of type Bool , it is feasible.
-}

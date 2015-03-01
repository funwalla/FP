{-
Chapter 1 exercises

Exercise 1.1 Write out all the steps in the calculation of the value of
       simple (simple 2 3 4) 5 6

simple x y z = x * (y + z)

simple (simple 2 3 4) 5 6
= simple (2 * (3 + 4)) * (5 + 6)
= simple (2 * 7) 5 6
= simple 14 5 6
= 14 * (5 + 6)
= 14 * 11
= 154

Exercise 1.2 Prove by calculation that simple (a - b) a b = a^2 - b^2

simple (a - b) a b
= (a - b) (a + b) = a^2 + a * b - b * a - b^2
= a^2 + a * b - a * b - b^2 = a^2 - b^2

Exercise 1.3 Identify the well-typed expressions in the following and, for each, give its proper type

[(2,3), (4,5)] :: [(Integer, Integer)]
Better: (Num t, Num t1) => [(t, t1)]

['z', 42]  Not well-typed, list members must all be of the same type

('z', -42) :: (Char, Integer)
Better: Num t => (Char, t)

simple 'a' 'b' 'c'  Not well typed, characters cannot be multipled and added.

(simple 1 2 3, simple) (Integer, Integer -> Integer -> Integer -> Integer)
Better: Num a => (a, a -> a -> a -> a)
Still better: (Num t, Num t1) => (t, t1 -> t1 -> t1 -> t1)
-}

simple :: Num t => t -> t -> t -> t
simple x y z = x * (y + z)

f [] = 0
f [5] = 999
f (x:xs) = x + f xs

           
        
sign x | x > 0  =  1
       | x == 0 =  0
       | x < 0  = -1
                  

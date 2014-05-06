crossProduct xs ys = [(x,y) | x <- xs, y <- ys ]

{- Exercise 2.5
What is the composition of {(n; n + 2), n <- N} (i.e., the natural numbers) with itself?

The relation maps each natural number, n, with n + 2. Composing the relation will map
each natural number with n + 4.

(n, n+2) -> (n+2, (n+2) + 2) = (n, n+2)-> (n+2, n+4) = (n, n+4)
i.e., the set {(n, n + 4), n <- N}


-}

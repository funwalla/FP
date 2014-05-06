square   :: Num a => a -> a
square x =  x * x

min' :: Ord a => a -> a -> a
min' x y | x <= y    = x
         | otherwise = y

side = 12
area = square side

{- Exercise 1.1.1 Using the function square, design a function quad
                  which raises its argument to the fourth power.
-}

quad   :: Num a => a -> a
quad x =  (square . square) x

{- Exercise 1.1.2 Define a function max which returns the greater
                  of its two arguments.
-}

max'     :: Ord a => a -> a -> a
max' x y |  x >= y    = x
         |  otherwise = y

{- Exercise 1.1.3

Define a function for computing the area of a circle with given radius r.
(use 22/7 as an approximation to pi)
-}

areaCircle :: Float -> Float
areaCircle r = (22/7) * (square r)

{- Exercise 1.2.1

Count the number of different ways that square(square(3+7)) can be reduced to
normal form.

1. Using left outermost lazy evaluation

    square(square(3+7)) => square(3+7) * square(3+7) =>
    (3+7) * (3+7) * square(3+7) => 10 * (3+7) * square(3+7) =>
    10 * 10 * square (3+7) => 100 * square(3+7) => 100 * (3+7) * (3+7) =>
    100 * 10 * (3+7) => 1000 * (3+7)  => 1000 * 10 => 10000.

2. Using innermost strict evaluation:

    square(square(3+7)) => square(square(10)) => square(10 * 10) =>
    square(100) => 100 * 100 => 10000.

Other mixes of the reduction orders are possible.
-}

{- Exercise 1.2.2

Consider the definition three x = 3. In how many ways can three(3+4) be reduced
to normal form?

1. Using left outermost lazy evaluation:

   three(3+4) => 3

2. Using innermost strict evaluation:

   three(3+4) => three(7) => 3
-}

{- Exercise 1.2.3

Imagine a language of expressions for representing integers defined by the
syntax rules:

  (1) zero is an expression;
  (2) if e is an expression then so are (succ e) and (pred e).

An evaluator reduces expressions in this language by applying the following
rules repeatedly until no longer possible:

  (succ(pred e)) => e    (succ.1)
  (pred(succ e)) => e    (pred.1)

Simplify the expression (succ(pred(succ(pred(pred zero)))))

outermost and innermost evaluation give the same result:
  (succ(pred(succ(pred(pred zero))))) => (succ(pred(pred zero))) =>
  (pred zero)
-}

-- § 1.4.2 Forms of definition

--f      :: (Ord a, Fractional a) => a -> a -> a
f x y  |  x > 10    = x + a
       |  otherwise = x - a
          where a = (x + y)/2

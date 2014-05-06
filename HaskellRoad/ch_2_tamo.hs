not'       :: Bool -> Bool
not' True   = False
not' False  = True

(∧)       :: Bool -> Bool -> Bool
True  ∧ x  =  x
False ∧ _  =  False

(∨)        :: Bool -> Bool -> Bool
True  ∨ _  =  True
False ∨ x  =  x

{- Exercise 2.2 Make up the truth table for the exclusive version of or.

+-------+-------+-------+
|   X   |   Y   |X  ⊕  Y|
+-------+-------+-------+
|   T   |   T   |   F   |
+-------+-------+-------+
|   T   |   F   |   T   |
+-------+-------+-------+
|   F   |   T   |   T   |
+-------+-------+-------+
|   F   |   F   |   F   |
+-------+-------+-------+
-}

infixr 2 ⊕    -- XOR

(⊕)  :: Bool -> Bool -> Bool
x ⊕ y = x /= y 

{- Implication (==>)

+-------+-------+-------+
|   X   |   Y   |X ==> Y|
+-------+-------+-------+
|   T   |   T   |   T   |
+-------+-------+-------+
|   T   |   F   |   F   |
+-------+-------+-------+
|   F   |   T   |   T   |
+-------+-------+-------+
|   F   |   F   |   T   |
+-------+-------+-------+

First implementation.
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_    ==> _     = True
-}


-- Book's implementation

infix 1 →  

(→) :: Bool -> Bool -> Bool
x → y = (not x) || y

{- Equivalence or Biconditional  (<==>)

+--------+--------+--------+
|   X    |   Y    |X <==> Y|
+--------+--------+--------+
|   T    |   T    |   T    |
+--------+--------+--------+
|   T    |   F    |   F    |
+--------+--------+--------+
|   F    |   T    |   T    |
+--------+--------+--------+
|   F    |   F    |   T    |
+--------+--------+--------+
-}

infix 1 <=>

(<=>)   :: Bool -> Bool -> Bool
x <=> y = x == y

{- Exercise 2.4

+----------+----------+----------+----------+
|    P     |    Q     | P xor Q  |¬(P <=> Q)|
+----------+----------+----------+----------+
|    T     |    T     |    F     |    F     |
+----------+----------+----------+----------+
|    T     |    F     |    T     |    T     |
+----------+----------+----------+----------+
|    F     |    T     |    T     |    T     |
+----------+----------+----------+----------+
|    F     |    F     |    F     |    F     |
+----------+----------+----------+----------+

-}

formula1 p q = (not p) && ((p → q) <=> not (q && not p))


valid2 bf = and [ bf p q | p <- vals, q <- vals]
            where vals = [True,False]

form1 p q = p → (q → p)
form2 p q = (p → q) → p


valid3 bf = and [ bf p q | p <- vals, q <- vals, r <- vals]
            where vals = [True,False]

valid4 bf = and [ bf p q | p <- vals, q <- vals, r <- vals, s <- vals]
            where vals = [True,False]

{- Exercise 2.9

XOR truth table:
+-------+-------+-------+
|   P   |   Q   |P  ⊕  Q|  (P ⊕ Q) ⊕ Q   
+-------+-------+-------+
|   T   |   T   |   F   |  (T F T) ⊕ T = T
+-------+-------+-------+
|   T   |   F   |   T   |  (T T F) ⊕ F = T
+-------+-------+-------+
|   F   |   T   |   T   |  (F T T) ⊕ T = F
+-------+-------+-------+
|   F   |   F   |   F   |  (F F F) ⊕ F = F
+-------+-------+-------+
-}

logEquiv1         :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =  (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

logEquiv2         :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 =  and [(bf1 p q) <=> (bf2 p q) | p <- vals, q <- vals]
                     where vals = [True, False]

logEquiv3         :: (Bool -> Bool -> Bool -> Bool) ->
                     (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 =  and [(bf1 p q r) <=> (bf2 p q r) | p <- vals,
                                                        q <- vals,
                                                        r <- vals]
                     where vals = [True, False]

-- redoing exercise 2.9:

formula3 p q = p
formula4 p q = (p ⊕ q) ⊕ q

ans29 = logEquiv2 formula3 formula4  -- = True, as before

-- Checking the equivalence directly using a biconditional:

formula5 p q = p <=> ((p ⊕ q) ⊕ q)

-- valid2 formula5 returns True.

{- Exercise 2.9   Use truth tables to prove each component of Theorem 2.10

See Onenote notebook

-}

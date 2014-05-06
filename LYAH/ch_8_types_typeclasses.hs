import qualified Data.Map.Lazy as Map

-- data Shape = Circle Float Float Float
--            | Rectangle Float Float Float Float deriving (Show)

-- surface :: Shape -> Float  
-- surface (Circle _ _ r) = pi * r ^ 2  
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
  (abs $ x2 - x1) * (abs $ y2 - y1) 

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday |
           Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

type AssocList k v = [(k, v)]

createAssocList     :: a -> b -> AssocList a b
createAssocList k v =  [(k, v)]

tstList = [(1,2),(3,5),(8,9)] :: AssocList Int Int

-- School locker example

-- requires Data.Map to be imported above

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup                  :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist."
    Just (state, code) -> if state /= Taken
                             then Right code
                             else Left $ "Locker " ++
                                         show lockerNumber ++
                                         " is already taken."

lockers :: LockerMap  
lockers = Map.fromList [ (100, (Taken, "ZD39I"))  
                       , (101, (Free,  "JAH3I"))  
                       , (103, (Free,  "IQSA9"))  
                       , (105, (Free,  "QOTSA"))  
                       , (109, (Taken, "893JJ"))  
                       , (110, (Taken, "99292"))  
                       ]

-- § Recursive data structures

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++)             :: List a -> List a -> List a
Empty .++ ys      =  ys
(x :-: xs) .++ ys =  x :-: (xs .++ ys)


-- Binary Search Trees

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton   :: a -> Tree a
singleton x =  Node x EmptyTree EmptyTree

treeInsert                                :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree                    =  singleton x
treeInsert x (Node a left right) | x == a =  Node x left right
                                 | x < a  =  Node a (treeInsert x left) right
                                 | x > a  =  Node a left (treeInsert x right)

treeElem                                :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree                    =  False
treeElem x (Node a left right) | x == a = True
                               | x <  a = treeElem x left
                               | x >  a = treeElem x right

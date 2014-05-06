import qualified Data.Map as Map

-- Algebraic data types intro

-- data Bool2 = False | True

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float |
             Rectangle Point Point
             deriving (Show)

surface                :: Shape -> Float
surface (Circle  _ r)                           =  pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =  (abs (x2 - x1)) * (abs (y2 - y1))

nudge                                           :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b                =  Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle   :: Float -> Shape
baseCircle r =  Circle (Point 0 0) r

baseRect              :: Float -> Float -> Shape
baseRect width height =  Rectangle (Point 0 0) (Point width height)

-- Record syntax

-- data Person = Person { firstName :: String  
--                      , lastName :: String  
--                      , age :: Int  
--                      , height :: Float  
--                      , phoneNumber :: String  
--                      , flavor :: String  
--                      } deriving (Show)

-- john = Person {firstName = "John", lastName = "Winchester",
--                age = 62, height = 70, phoneNumber = "206-819-0336",
--                flavor = "chocolate"}

-- describePerson :: Person -> String
-- describePerson (Person{firstName = f, lastName = l, age = a}) = "Name: " ++ f ++ " " ++ l ++ ", Age: " ++ show a

-- Type parameters

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t  -> Vector t
(Vector i j k) `vplus` (Vector l m n) =  Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m =  Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t  -> t
(Vector i j k) `scalarMult` (Vector l m n) =  (i * l) + (j * m) + (k * n)

-- Derived instances

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)

john    = Person {firstName = "John"   , lastName = "Winchester", age = 62}
dianne  = Person {firstName = "Dianne" , lastName = "Stangl",     age = 48}
dianne2 = Person {firstName = "Dianne" , lastName = "Stangl",     age = 48}

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  

phoneBook :: PhoneBook
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ] 

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook  

-- The Either type

-- "import qualified Data.Map as Map"" inserted at top of file.

data LockerState = Taken | Free deriving (Show, Eq)  
type Code = String  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
                                     Nothing            -> Left $ "Locker number "
                                                                  ++ show lockerNumber
                                                                  ++ " doesn't exist"
                                     Just (state, code) -> if state /= Taken then Right code
                                                           else Left $ "Locker "
                                                                       ++ show lockerNumber
                                                                       ++ " is taken."

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

--data List1 a = Empty | Cons a (List1 a) deriving (Show, Read, Eq, Ord)

-- infixr 5 :-:  
-- data List2 a = Empty | a :-: (List2 a) deriving (Show, Read, Eq, Ord)  

-- infixr 5 .+.
-- (.+.)             :: (List2 a) -> (List2 a) -> (List2 a)
-- Empty .+. ys      =  ys
-- (x :-: xs) .+. ys =  x :-: (x .+. ys)

-- Binary Trees

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
            deriving (Show, Read, Eq)
                     
singleton   :: a -> Tree a
singleton x =  Node x EmptyTree EmptyTree

treeInsert             :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree =  singleton x
treeInsert x (Node a left right) | x == a = Node x left right
                                 | x  < a = Node a (treeInsert x left) right
                                 | x  > a = Node a left (treeInsert x right)

treeElem             :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree                    = False
treeElem x (Node a left right) | x == a = True
                               | x  < a = treeElem x left
                               | x  > a = treeElem x right
                                          
nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

-- Typeclasses 102

class Eq' a where
  (.==.) :: a -> a -> Bool
  (./=.) :: a -> a -> Bool
  x .==. y = not (x ./=. y)
  x ./=. y = not (x .==. y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red    == Red     = True
  Green  == Green   = True
  Yellow == Yellow  = True
  _      == _       = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

-- Functors

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

class Tofu t where  
    tofu :: j a -> t a j 

data Frank a b  = Frank {frankField :: b a} deriving (Show)

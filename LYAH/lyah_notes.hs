doubleMe x = x + x

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

circumference :: Float -> Float  

circumference r = 2 * pi * r 

circumference' :: Double -> Double
circumference' r = 2 * pi * r 

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"
charName _   = "Catch all"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

tell :: (Show a) => [a] -> String
tell []          = "The list is empty"
tell (x:[])      = "the list has one element: " ++ show x
tell (x:y:[])    = "the list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_)     = "the list is long. First two elements: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' []         = 0
length' (_:xs)     = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []         = 0
sum' (x:xs)     = x + sum' xs


max' a b   
    | a > b     = a  
    | otherwise = b  

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Overweight"
  | otherwise   = "Whale"
  where bmi = weight / height ^ 2

initials :: String -> String -> String
initials fname lname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = fname
        (l:_) = lname

calcBMIs :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

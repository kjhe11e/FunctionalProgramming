removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


-------------------------------------------
--PATTERN MATCHING
-------------------------------------------

--important to include "catch-all" clause
--evaluated top-to-bottom
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY SEVEN"
lucky x = "Number 7 not entered."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

--for tuples:
first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

myHead :: [a] -> a
myHead [] = error "Cannot call head on an empty list."
myHead (x:_) = x

myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

firstLetterString :: String -> String
firstLetterString "" = "Empty string, error."
firstLetterString wholeString@(x:xs) = "The first letter of " ++ wholeString ++ " is " ++ [x]

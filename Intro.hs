removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


----------------------------------------------------
--PATTERN MATCHING
-- makes sure value conforms to form and decomposes
----------------------------------------------------

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

--------------------------------------
-- GUARDS
--------------------------------------

heightTell :: (RealFloat a) => a -> String
heightTell ht
	| ht <= 60 = "Short"
	| ht <= 65 = "Slightly short"
	| ht <= 70 = "Average height"
	| ht <= 74 = "Above average height"
	| otherwise = "Tall"


myMax :: (Ord a) => a -> a -> a
myMax a b
	| a > b 	= a
	| otherwise = b


myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b 		= GT
	| a == b 		= EQ
	| otherwise 	= LT 

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell wt ht  
    | bmi <= skinny = "Underweight"  
    | bmi <= normal = "Normal"  
    | bmi <= fat    = "Above"  
    | otherwise     = "Very high BMI"  
    where bmi = wt / ht ^ 2  
          skinny = 18.7  	--note, these names only visibile to this function
          normal = 26.0  
          fat = 34.0


returnInitials :: String -> String -> String  
returnInitials firstname lastname = [f] ++ "." ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname


--case expressions: if no match caught, then runtime error occurs
headList :: [a] -> a  
headList xs = case xs of [] -> error "Empty list does not have head."  
                         (x:_) -> x  

--recursion:
maxOfList :: (Ord a) => [a] -> a  
maxOfList [] = error "Empty list."  
maxOfList [x] = x  
maxOfList (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maxOfList xs  






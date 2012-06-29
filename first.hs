import Data.List
import qualified Data.Map as Map

-- First lines of haskell code ever!!
doubleMe x = x+x
sumList (x:xs) = x + sumList xs
sumList [] = 0

myFirstName = "Matthew"
myLastName = "Clemens"
{- the ++ adds lists together
 - Carefull of performance though
 - it has to walk the list to add to the end -}
fullName = myFirstName ++ " " ++ myLastName

-- the : is called the "cons" operator
-- adds to the front of a list
initals = 'M' : "DC"

-- creating a simple list
favoriteLangauges = ["Python", "Javascript", "C", "Haskell", "Java"]
-- above was just syntax sugar for doing this below
favoriteNumbers = 24:66:13:6624:[]

-- length must traverse whole list


-- getting a single element from a list 
mostFavoriteNumber = favoriteNumbers !! 0

nestedListEx = [[0,1],[1,0]]

-- creating lists from ranges
yearsOfMyLife  = [0..21]

listOfEvens = [2,4..20]
infiniteEvens = [2,4..]

stutter x = take x  (repeat "Chill Bro ")

-- Pattern Matching example 
--  !remember to finish or start with a "catch all pattern 
numberHistory :: (Integral a) => a -> String
numberHistory 24 = "Favorite Sports number"
numberHistory 66 = "2nd Favorite Sports number"
numberHistory 13 = "Lucky Number thirteen"
numberHistory 6624 = "How did you know?"
numberHistory x = "Not too found of that number"

-- Integral is a typeclass
-- the (Integral a) => part of the signiture is signalling the
-- typeclass that this function can work with
factorial ::(Integral a) => a -> a 
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Pattern Matching example with tuples
-- by reimplementing the standard head function
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x

-- Polymorphic type in a type signiture
someFakeFunction :: a -> a
-- implies that the some type of b doesnt have to be the
-- same type as a
someFakeFunction3 :: a -> b


-- Pattern matching lists 
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

-- quick tutorial sort
{- However this is not effiecent with arrays being copied 
 - in every func, gc will dominate if the array is non random 
 - in its ordering?, plus the list addition overhead -}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where (lesser, greater) = partition (< p) xs

-- Showing that pattern matching in function defintions is just
-- syntactic sugar for case expressions
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

-- same as above
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."

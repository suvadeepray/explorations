module Kaprekar(kaprekar,kaprekarlist,split,joinWith)
    where

import Data.Char

-- splits given integer into digits
split::Int->[Int]
split n = map digitToInt (show n)

-- joins given list of digits to form single integer 
join::[Int]->Int
join digits = joinWith digits 10
joinWith digits  n = sum (placeValues digits)
    where
	placeValues::[Int]->[Int]
	placeValues [] = []
	placeValues (x:xs) = 
	    let
		tensValue = length (x:xs) -1
		value = x*(n^tensValue)
	    in(value:placeValues xs)

-- quicksort..its too quick to type and too expressive too!
quicksort::[Int]->[Int]
quicksort [] = []
quicksort (x:xs) = quicksort more ++ [x] ++ quicksort less
    where
	more = filter (>=x) xs
	less = filter (<x) xs

-- padds zeros to start of list and make the list to given size
paddZeros::[Int]->Int->[Int]
paddZeros [] k = zeros k
paddZeros x k = zeros (k -length(x)) ++ x

-- zeros
zeros::Int->[Int]
zeros k = map (*0) [1..k] 

-- performs kaprekar operation on given number for given number of digits
kaprekar::Int->Int->Int
kaprekar n numDigits = 
    let
	digits = paddZeros (split n) numDigits
	max = join (quicksort digits)	
	min = join (reverse (quicksort digits))
    in (max - min)

-- performs kaprekar operation iteratively
kaprekaritr::[Int]->Int->Int->[Int]
kaprekaritr [] n k = []
kaprekaritr (x:xs) n k = itr:kaprekaritr xs itr k
    where
	itr = kaprekar n k

-- Returns results of first numitr iterations of kaprekar operation
kaprekarlist::Int->Int->Int->[Int]
kaprekarlist n k numitr = kaprekaritr [1..numitr] n k

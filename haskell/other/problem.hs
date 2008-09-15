module Main
    where
import Kaprekar


baseSevenDigits::Integer->[Integer]->[Integer]
baseSevenDigits n  
	| n < 7 = [n]
	| otherwise = baseSevenDigits (div n 7) ((mod n 7):xs)


maxCheckZeros x:xs = foldl f 0 xs
    




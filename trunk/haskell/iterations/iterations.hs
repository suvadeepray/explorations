module Iterations
    where

riffleshuffle::[a]->[a]
riffleshuffle xs = merge (take half xs) (drop half xs)
    where
	half = (length xs) `div` 2

merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x:y:merge xs ys

shuffle xs n 
    | n < 1 = xs
    | otherwise = shuffle (riffleshuffle xs) (n-1)


shuffleitr xs n
    | xs == [1..(length xs)] = n
    | otherwise  = shuffleitr (riffleshuffle xs) n+1

countShufle m = shuffleitr (riffleshuffle [1..m]) 1

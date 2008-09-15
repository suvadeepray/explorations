module Main
    where


comb n k = generateCombinations [1..n] k

g x = [x]
generateCombinations::[Int]->Int->[[Int]]
generateCombinations (x:xs) k
    | k == 1 = map g (x:xs)
    | k > (length (x:xs)) = []
    | otherwise  = 
	[x:ys | ys<-generateCombinations xs (k-1)] ++ generateCombinations xs k
	{--
	let
	    f l = x:l
	in((map f (generateCombinations xs (k-1))) ++ (generateCombinations xs k))
	--}
	
--main=putStrLn (show (comb 5 3) )

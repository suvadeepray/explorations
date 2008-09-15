module Main
	where

-- Usual recursive fibonaci
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

f n 
    | n ==1 = 1
    | n ==2 = 1
    | otherwise = fib (n-1) (n-2)

-- fibonacci sequesnce generator
fibseries fibs n =
	if length fibs < n 
		then fibseries ((fibs!!0 + fibs!!1):fibs) n
		else fibs

-- efficient fibonacci
fibonacci n = 
	head (fibseries [1,1] n)


fibiter a b p q count 
    | count == 0 	= b
    | odd count  	= fibiter (b*q + a*q + a*p) (b*p + a*q) p q (count -1)
    | otherwise 	= fibiter a b p1 q1 (count `div` 2)
	    where 
		p1 = p*p + q*q
		q1 = q*q + 2*p*q


efficientfib = fibiter 1 0 0 1

main= putStrLn $ show $ fibonacci 1000000

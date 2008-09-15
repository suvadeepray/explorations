module Main
    where


--Add all the natural numbers below 1000 that are multiples of 3 or 5.{{{
s = sum (filter f [1..1000])
    where
	f x = (x `mod` 3 == 0) || (x `mod` 5 ==0)

s1 = sum ([x | x<-[1..1000], x `mod` 3 ==0 || x `mod` 5==0])
--}}}

--Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed one million. --{{{
fibs = 1:1:[a+b | (a,b) <- zip fibs (tail fibs)]
cutoff n (x:xs) 
    | x <= n = x:(cutoff n xs)
    | otherwise = []

fib = take (((length cfibs) `div` 3)*3) cfibs
	where
	    cfibs = cutoff 1000000 fibs
fibsum = (sum fib)/2

--}}}

-- Find the largest prime factor of 317584931803.
divisors a = [x | x<-[2..a], mod a x == 0]
firstLargestFactor a = largestFactor a (divisors a)
    where
	largestFactor a [] = a
	largestFactor a (x:xs)
	    | a == x 	= a
	    | otherwise = a `div` x

factorItr p f 
    | p == f 	= p
    | otherwise = factorItr f (firstLargestFactor f)

firstLargestPrimeFactor a = factorItr a (firstLargestFactor a)


factors a facs = (firstLargestPrimeFactor a):facs




-- problem 176
let l = [(a,b,c,d,e,f,p,q,r,m,n,o) | a <- [1..179], b <- [1..179], c <- [1..179], p <- [1..179], q <- [1..179], r <- [1..179], x <- [1..179], y <- [1..179], z <- [1..179], m <- [1..179], n <- [1..179], o <- [1..179],x+y+z==180,a+b+c==180,p+q+r==180,m+n+o==180,y+a==q,z+b==n,p+m==x,r+o==c]


-- vim: foldmethod=marker

module Functions
    where

square x = x*x

signum x =
    if x < 0
	then -1
	else if x > 0
	    then 1
	    else 0


f x =
    case x of
	0 -> 5
	1 -> 4
	2 -> 3
	3 -> 2
	4 -> 1
	5 -> 0
	_ -> -1  --this is comment

roots a b c=
    let det = sqrt (b*b -4*a*c)
	twice_a = 2*a
    in ((-b+det)/twice_a,
	(-b-det)/twice_a)
	{-this is multiline
	comment
	-}

-- factorial
factorial 0 = 1
factorial n = n * factorial (n-1)
-- factorial end

-- exponent
myexponent a 0 = 1
myexponent a n = a * myexponent a (n-1)
-- exponent end

-- length of list
mylength [] = 0
mylength (x:xs) = 1 + mylength xs
-- end


-- filter --
myfilter p [] = []
myfilter p (x:xs)=
    if p x
	then x:(myfilter p xs)
	else (myfilter p xs)
-- end --

-- fib --
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2) {- this is going to be terrifc slow for large values of n!
				 guess why?
			      -}
-- end --

-- mul --
mul a 1 = a
mul a b = a + mul a (b-1)
-- end --

-- mymap --
mymap op [] = []
mymap op (x:xs) = op x :(mymap op xs)
-- end --

--qsort
qsort [] = []
qsort (x:xs) = qsort less ++ [x] ++ qsort more
    where
	less = filter (<x) xs
	more = filter (>=x) xs
--end


test x n = filter (testMod n) x
    where 
	testMod n y = (mod y n /= 0)

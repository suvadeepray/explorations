module Whyfp
    where

-- defining function in haskell is very esay
--sqr::a->a  -- sqr is function which takes an argument of generic type
	   -- and returns a value of same type
--sqr x = x*x

isqr::Integer->Integer -- a function that takes Integer argument and returns Integer

isqr i = i*i
--sqr::a->a
--sqr x = x*x

reduce = foldr -- for time being forget about what is this!

{-
data list a = Nil | cons a (list a)

examples

[] 		... Nil
[1] 		... cons 1 Nil
[1,2,3,4]	... cons 1 (cons 2 (cons 3 (cons 4 Nil))
		... alternatively 1:[2,3,4], 1:2:3:4:[]
-}		

-- haskell way replace cons by an operator (:)
-- [] 		... []
-- [1]		... 1:[]  or (:) 1 []
-- [1,2,3,4]	... 1:2:3:4:[]

-- sum of all elements in a list

listsum [] = 0
listsum (x:xs) = x + listsum xs

listproduct [] = 1
listproduct (x:xs) = x * listproduct xs

{-
reduce f x [] = x
reduce f x (y:ys) = f y (reduce f x ys) 

reduce f 0 [1..4] = f 1 (reduce f 0 [2..4])
		    f 1 (f 2 (reduce f 0 [3,4]))
		    f 1 (f 2 (f 3 (reduce f 0 [4]))
		    f 1 (f 2 (f 3 (f 4 (reduce f 0 []))))
		    f 1 (f 2 (f 3 (f 4 0)))
-}

listsum_g = reduce add 0
listproduct_g = reduce multiply 1

add x y = x+y
multiply x y = x*y

{-
doubleall = foldr doubleandcons []
    where doubleandcons num list = 2*num:list


doubleandcons = fandcons double
    where 
	double n = 2*n
	fandcons = (f el):list
-}	


-- Ackermann's function

ackermann x y 
    | y==0  	= 0
    | x==0  	= 2*y
    | y==1  	= 2
    | otherwise = ackermann (x-1) (ackermann x (y-1))

-- ackermann 0 1 = 2	-- multiplication
-- ackermann 0 2 = 4
-- ackermann 0 3 = 6
-- 2*y = y times add

-- ackermann 1 1 = 2
-- ackermann 1 2 = 4
-- ackermann 1 3 = 8
-- ackermann 1 4 = 16
--2**y = y time *
--
-- ackermann 2 0 = 

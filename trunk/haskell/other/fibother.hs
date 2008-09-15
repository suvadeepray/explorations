module Main
	where
fibs = 1 : 1 : [ a+b | (a,b) <- zip fibs (tail fibs) ]
fibf n = fibs !! n
nfibs n = take n fibs

f::Integer->Integer

f n = fibhelper n 1 1
    where
	fibhelper 1 a _ = a
	fibhelper 2 _ b = b
	fibhelper n a b = fibhelper (n-1) b (a+b)

main = putStrLn (show (f 100000))

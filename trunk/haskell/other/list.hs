module MyList
	where

lastElement (x:xs)=
    if length xs == 0
	then x
	else lastElement xs

mreverse (x:xs)=
	if length xs ==0
	    then x:[]
	    else let last = lastElement (x:xs)
	         in( last:mreverse (take (length xs) (x:xs)))

range n = [1..n]


cons8 (x:xs) = 8:(x:xs)



module HigherOrder
    where

mylength xs = foldr op 0 xs
    where op _ s = s+1

{--
map     (a->b)->[a]->[b]
map map   (c->d)->[c]->[d]
	   c = (a->b)
	   d = [a]->[b]
map map :: [a->b]->[[a]->[b]]


(.) ::(b -> c) -> (a -> b) -> a -> c
map.map  = 
	map::(a->b)->[a]->[b]
	b = (a->b) c=[a]->[b]
	a=(a->b)  b=[a]->[b]

	a->c = (a->b)->[a]->[b]

foldl (a->a->a)->a->[a]->a

map foldl (c->d)->[c]->[d]
	c=  a->a->a
	d= a->[a]->a
map foldl :: [a->a->a]->[a->[a]->a]    correct!
	  :: [(a->a->a)->a]->[[a]->a]  ??? wrong!
	
--}



doubleEach xs = map doubleit xs
    where 
	doubleit x = 2*x

doubleEachr [] = []
doubleEachr (x:xs) = 2*x:doubleEachr xs

pairAndOne xs = zip xs (tail xs)

pairAndOner (x:y:[]) = [(x,y)]
pairAndOner (x:xs) = (x,(head xs)):pairAndOner xs

addEachPair xs = map addPair xs
    where 
	addPair (x,y) = x+y

addEachPairr [] = []
addEachPairr ((x,y):xs) = (x+y):addEachPairr xs

maxList xs = foldl max (head xs) xs
minList xs = foldl min (head xs) xs

maxListr (x:y:[]) = max x y
maxListr (x:xs) = max x (maxListr xs)

minListr (x:y:[]) = min x y
minListr (x:xs) = min x (minListr xs)


addPairPointwise xs = foldl addPoint (0,0) xs --{{{
    where
	addPoint (x,y) (m,n) = (x+m,y+n)

addPairPointwiser ((x,y):(m,n):[]) = (x+m,y+n)
addPairPointwiser ((x,y):xs) = (x + (fst (addPairPointwise xs)), y + (snd (addPairPointwise xs)))
--}}}

-- {{{ encryption sustem for frog
crypt doWhat chars =  --{{{
    let
	intValues = map charToInt chars
	ints = map doWhat intValues
    in (map intToChar ints)
--}}}

charToInt::Char->Int
charToInt c =  fromEnum c
intToChar::Int->Char
intToChar i = toEnum i

add_ x			--{{{ 
	| x == 255 = 0
	| otherwise = x+1
--}}}

subtract_ x 		--{{{
	| x == 0 = 255
	| otherwise = x-1
--}}}

encrypt::[Char]->[Char]	--{{{
encrypt cs = crypt add_ cs
--}}}

decrypt::[Char]->[Char]	--{{{
decrypt cs = crypt subtract_ cs
--}}}

--}}}


findChanger::Int->[Int]->[Int]
findChanger _ [] = []
findChanger amt (c:cs) = numCoins:findChange remainingAmt cs
	    where
		numCoins = amt `div` c
		remainingAmt = amt `mod` c

-- vim: foldmethod=marker

module Main
    where

class MyEq a where
    (==)::a->a->Bool


newtype Natural = MakeNatural Integer


data MyList a = EmptyList | (:+) a (MyList a)

{--
integerEq::Integer->Integer->Bool
integerEq x y = x==y

instance Eq Natural where
    x == y  = (fromNatural x) `integerEq` (fromNatural y)
--}
data YANatural = MakeNatural1 Integer

toNatural::Integer->Natural
toNatural x | x < 0	= error "Can't create negative naturals!"
	    | otherwise = MakeNatural x

fromNatural::Natural ->Integer
fromNatural (MakeNatural i) = i

toNatural1::Integer->YANatural
toNatural1 x | x < 0	= error "Can't create negative naturals!"
	     | otherwise = MakeNatural1 x

fromNatural1::YANatural->Integer
fromNatural1 (MakeNatural1 i) = i


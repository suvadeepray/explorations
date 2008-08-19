module Tree
    where

data Tree a = Leaf a | Branch (Tree a) (Tree a) --{{{


mapTree::(a->b)->Tree a->Tree b

mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch left right) = Branch (mapTree f left) (mapTree f right)

fringe :: Tree a -> [a]

fringe (Leaf x) = [x]
fringe (Branch left right) = fringe left ++ fringe right

treeSize :: Tree a -> Int
treeSize (Leaf x) = 1
treeSize (Branch left right) = treeSize left + treeSize right

treeHeight :: Tree a -> Int

treeHeight (Leaf x) = 1
treeHeight (Branch left right) = 1 + max (treeHeight left) (treeHeight right)


foldTree f val (Leaf x) = f (Leaf x) val 
foldTree f val (Branch left right) = foldTree f (foldTree f val right) left

redTree f g a (Leaf x) = f x a
redTree f g a (Branch left right) = 
	g (redTree f g a left) (redTree f g a right)

fringex t = redTree (:) (++) [] t  
sizex t = redTree f g 0 t
    where
	f _ val = 1 + val
	g a b = a + b

height t = 1 + (redTree f g 1 t)
    where 
	f _ _ = 0
	g a b = 1 + (max a b)

-- fringe using higher order function
fringeh::Tree a->[a]
fringeh t = foldTree f [] t
    where f (Leaf x) list = x:list

-- treesize using higher order function
treeSizeh t = foldTree f 0 t
    where f (Leaf x) val = 1 + val
--}}}

{-
data Treee a = Leaf a | Branch (Treee a) a (Treee a)

treeeSize (Leaf x) = 1
treeeSize (Branch left x right) = 1 + treeeSize left + treeeSize right
-}

instance (Eq a) => Eq (Tree a)
    where 
	Leaf a         == Leaf b          =  a == b
	(Branch l1 r1) == (Branch l2 r2)  =  (l1==l2) && (r1==r2)
	_              == _               =  False


t11 = Branch (Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)) ) (Branch (Leaf 4) (Leaf 5) ) ) (Leaf 6)
t13 = Branch (Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)) ) (Branch (Leaf 4) (Leaf 5) ) ) (Leaf 6)
t22 = Branch (Branch (Branch (Leaf 1) (Branch (Leaf 2) (Branch (Leaf 3) (Leaf 3) )) ) (Branch (Leaf 4) (Leaf 5) ) ) (Leaf 6)

showTree                :: (Show a) => Tree a -> String
showTree (Leaf x)       =  show x
showTree (Branch l r)   =  "<" ++ showTree l ++ "|" ++ showTree r ++ ">"



data InternalTree a = 	  ILeaf --{{{
			| IBranch a (InternalTree a) (InternalTree a) 
	deriving Show

takeTree::Integer->InternalTree a->InternalTree a

takeTree _ ILeaf = ILeaf
takeTree n (IBranch x left right) 
    | n == 0 	= ILeaf
    | otherwise = IBranch x (takeTree (n-1) left) (takeTree (n-1) right)

ifoldl f g val (IBranch x left right) = 
	f x (g (ifoldl f g val left) (ifoldl f g val right))
ifoldl f g val ILeaf = val

ifringe::InternalTree a->[a]
ifringe t = ifoldl (:) (++) [] t

it1 = IBranch 1 (IBranch 2 (IBranch 3 (IBranch 4 ILeaf ILeaf) (ILeaf) ) (IBranch 5 ILeaf ILeaf) ) (IBranch 8 (IBranch 6 ILeaf (IBranch 7 ILeaf ILeaf) ) ILeaf)
--}}}

printTree::(Show a)=>InternalTree a->Int->IO()
printTree (ILeaf) n = putStrLn "ILeaf"
printTree (IBranch x left right) n = 
    do --putStrLn $ (take n $ repeat ' ') ++ "IBranch " ++ (show x)
       putStrLn (show x)
       printTree left (n+1)
       printTree right (n+1)
       
-- vim: foldmethod=marker

module List
    where


data List a = Nil 
	    | Cons a (List a)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs


listHead Nil = error "Empty List!"
listHead (Cons x xs) = x


listTail Nil = error "Empty List!"
listTail (Cons x xs) = xs

listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f x v) xs



listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)

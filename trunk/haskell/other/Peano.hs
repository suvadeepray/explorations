module Peano
    where


data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)

one = Succ Zero
two = Succ one
three = Succ two
four = Succ three

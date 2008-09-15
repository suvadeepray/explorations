module Weave 
    where
import Data.List

weave =
  unfoldr f
     where
        f ([],_,_)     = Nothing
        f (x:xs,[],zs) = Just (x,([],[],[]))
        f (x:xs,ys,zs) = Just (x,(ys,zs,xs))

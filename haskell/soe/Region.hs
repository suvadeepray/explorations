module Region(	Region (Shape, Translate, Scale, Complement,
			Union, Intersect, Empty),
		Coordinate,
		containsS, containsR,
		module Shape
	     ) where

import Shape

infixr 5 `Union`
infixr 6 `Intersect`

data Region = Shape Shape		-- primitive type
	    | Translate Vector Region 	-- translate region
	    | Scale Vector Region	-- scale region
	    | Complement Region		-- inverse of region
	    | Region `Union` Region	-- union of regions
	    | Region `Intersect` Region	-- intersection of regions
	    | Empty
    deriving Show

type Vector = (Float, Float)

type Coordinate = (Float, Float)

containsR :: Region -> Coordinate -> Bool

(Shape s) `containsR` p
          = s `containsS` p

(Translate (u,v) r) `containsR` (x,y)
                    = r `containsR` (x-u, y-v)

(Scale (u,v) r) `containsR` (x,y)
                = r `containsR` (x/u, y/v)

(Complement r) `containsR` p
               = not ( r `containsR` p)

(r1 `Union` r2) `containsR` p
                = r1 `containsR` p || r2 `containsR` p

(r1 `Intersection` r2) `containsR` p
                       = r1 `containsR` p && r2 `containsR` p

Empty `containsR` p
      = False

containsS :: Shape -> coordinate -> Bool

(Rectangle s1 s2) `containsS` (x,y)
                  = let t1 = s1/2
                        t2 = s2/2
                    in -t1 <= x && x <=t1 && -t2 <=y && y <=t2

(Ellipse r1 r2) `containsS` (x,y)
                = (x/r1)^2 + (y/r2)^2 <=1

(Polygon pts) `containsS` p
              = let leftOfList = map (isLeftOf p) (zip pts (tail pts++[head pts]))
                    isLeftOf p' = isLeftOf p p'
                in and leftOfList 

(RtTriangle s1 s2) `containsS` p
                   = (Polygon [(0,0),(s1,0),(0,s2)]) `containsS` p

--}}}


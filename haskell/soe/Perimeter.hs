module Perimeter(perimeter,
		 module Shape
		)where
import Shape
perimeter::Shape->Float


sidesr::[Vertex]->[Side]
sidesr [] = []
sidesr (v:vs) = aux v vs
    where aux v1 (v2:vs') = distBetween v1 v2:aux v2 vs'
	  aux vn [] = distBetween vn v:[]

sides::[Vertex]->[Side]
sides vs = zipWith distBetween vs (tail vs ++ [head vs])

epsilon = 0.0001::Float

perimeter (Rectangle s1 s2) = 2 * (s1 + s2)
perimeter (RtTriangle s1 s2) = s1 + s2 + sqrt (s1^2 + s2^2)
perimeter (Polygon vs) = foldl (+) 0 (sides vs)
perimeter (Ellipse r1 r2)
    | r1 > r2 = ellipsePerim r1 r2
    | otherwise = ellipsePerim r2 r1
    where ellipsePerim r1 r2
	    = let e 	  = sqrt (r1^2-r2^2)/r1
		  s 	  = scanl aux (0.25*e^2) [2..]
		  aux s i = nextEl e s i
		  test x  = x > epsilon
		  sSum = foldl (+) 0 (takeWhile test s)
	      in 2*r1*pi*(1-sSum)

nextEl::Float->Float->Float->Float
nextEl e s i = s * (2 * i -1) * (2 * i -3) * (e^2)/(4*i^2)


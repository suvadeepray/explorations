module Shape
    where


data Shape  = Rectangle Side Side
	    | Ellipse Radius Radius
	    | RtTriangle Side Side
	    | Polygon [Vertex]
	deriving Show

type Side	= Float
type Radius 	= Float
type Vertex	= (Float, Float)
type Area	= Float
type Distance	= Float

--Shapes {{{

rectangle::Side->Side->Shape--{{{ rectangle derived from Polygon
rectangle s1 s2 = Polygon [(-s1/2,-s2/2),(s1/2,-s2/2),(s1/2,s2/2),(-s1/2,s2/2)]
-- rectangle end}}}

rtTriangle::Side->Side->Shape --{{{right angled trianle using polygon 
rtTriangle s1 s2 = Polygon [(0.0,0.0),(s1,0.0),(s1,s2)]
-- end right angle triangle }}}

circle::Radius->Shape--{{{
circle r = Ellipse r r
--}}}

regularPolygon::Side->Int->Shape --{{{ regular polygon
regularPolygon s n = 
    let 
	polyVertices =  zip x y
	x = polyCoordinate r n cos
	y = polyCoordinate r n sin
	r = s/(2 * sin (piByn))
	piByn = pi / (intToFloat n)

	polyCoordinate r n func = map f [0..(n-1)]--{{{
	    where
		f::Int->Float
		f m = r * (func (floatm*2*piByn))
		    where 
			floatm = intToFloat m
	--}}}

	intToFloat :: Int->Float--{{{
	intToFloat n = fromInteger (toInteger n)
	--}}}
     in(Polygon polyVertices)
--}}}

--}}}

--Area {{{

area::Shape->Area --{{{

area (Rectangle s1 s2) = s1*s2
area (RtTriangle s1 s2) = s1*s2/2
area (Ellipse r1 r2) = pi*r1*r2

area (Polygon (v1:vs)) = polyArea vs
    where 
	polyArea::[Vertex]->Area

	polyArea (v2:v3:vs) = triArea v1 v2 v3 + polyArea (v3:vs)
	polyArea _ = 0
--}}}

areaNonRecursive::Shape->Area--{{{
areaNonRecursive (Polygon vs) = 
    let
	triangles = subtriangles vs
	triangleArea::(Vertex,Vertex,Vertex)->Area
	triangleArea (x,y,z) = triArea x y z
    in (foldr (+) 0 (map triangleArea triangles))
areaNonRecursive _ = 0
--}}}

subtriangles::[Vertex]->[(Vertex,Vertex,Vertex)]--{{{
subtriangles vs = map f vspairs
	where
	    vspairs = zip (tail vs) tailoftail
	    tailoftail = tail (tail vs)
	    f (x,y) = (head vs,x,y)
--}}}

triArea::Vertex->Vertex->Vertex->Area --{{{

triArea v1 v2 v3 = sqrt (s*(s-a)*(s-b)*(s-c))
    where
	a = distBetween v1 v2
	b = distBetween v2 v3
	c = distBetween v3 v1
	s = 0.5 * (a + b + c)
--}}}

distBetween::Vertex->Vertex->Distance --{{{
distBetween (x1,y1) (x2,y2) = sqrt( (x1-x2)^2 + (y1-y2)^2 )
--}}}

--}}}

--other {{{

--convex::Shape->Bool--{{{
convex (Polygon (v1:v2:v3:vs)) = (isLessThanPiList (v1:v2:v3:vs))
    where
	isLessThanPiList [v1,v2,v3] = [isLessThanPi v1 v2 v3]
	isLessThanPiList (v1:v2:v3:vs) = ((isLessThanPi v1 v2 v3):isLessThanPiList (v2:v3:vs))
	isLessThanPi v1 v2 v3  = (angle v1 v2 v3) 
	angle v1 v2 v3 = 
	    let 
		theta = angle1 - angle2
		angle1 = slope v2 v1
		angle2 = slope v2 v3
		slope (x1,y1) (x2,y2) = atan (y2-y1)/(x2-x1)
	    in(if theta < 0 
		    then (2*pi+theta) 
		    else theta
	    )
--}}}

--}}}

-- vim: foldmethod=marker

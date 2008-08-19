module Main
    where

import Graphics.HGL

intToFloat :: Int->Float
intToFloat n = fromInteger (toInteger n)

heightToSide::Int->Int
heightToSide h = round (2 * (abs (intToFloat h)) / (tan (pi/3)) )

triangle::Int->Int->Int->[Point]
triangle x y h = (x,y):(x2,y):(x3,y3):[]
    where
	x2 = x + heightToSide h
	x3 = (x + x2) `div` 2
	y3 = y - h

invertTriangle::[Point]->[Point]
invertTriangle [(x1,y1),(x2,y2),(x3,y3)]
    = (x1,yupper):(x2,yupper):(x3,ylower):[]
	where
	    yupper = y1 - 2*h
	    ylower = y1 + h
	    h = (y1 - y3) `div` 3

fillTri::Window->[Point]->Color->IO()
fillTri w vertices color =
	drawInWindow w ( withColor color
	    (polygon vertices))

colors::[Color]
colors = [Yellow,White,Cyan,Blue,Green]
--colors = [White,Yellow,Red,Magenta,Cyan]

subTraingles::[Point]->[[Point]]
subTraingles [(x1,y1),(x2,y2),(x3,y3)]=
    let	p1 = (x3,y3) 
	p2 = (x1,y3+h)
	p3 = (x1+s,y3+h)
        p4 = (x1+2*s,y3+h)
	p5 = (x2,y3+h)
	p6 = (x1+ s `div` 2,y3+2*h)
	p7 = (x2- s `div` 2,y3+2*h)
	p8 = (x1,y1)
	p9 = (x1+s,y1)
	p10 = (x2-s,y1)
	p11 = (x2,y2)
	p12 = ( (x1+x2) `div` 2, y1+h)
	s = (x2 - x1) `div` 3
	h = (y1 - y3) `div` 3
    in [[p3,p4,p1],[p2,p3,p6],[p4,p5,p7],[p8,p9,p6],[p10,p11,p7],[p9,p10,p12]]

fillTriList::Window->[[Point]]->[IO()]
fillTriList w [] = []
fillTriList w (x:xs) = fillTri w x Blue:fillTriList w xs

snowFlakeList::Window->[[Point]]->[Color]->[IO()]
snowFlakeList w [] _ = []
snowFlakeList w (x:xs) color = snowFlake w x color:snowFlakeList w xs color

maxLevel::Int
maxLevel = 5
snowFlake::Window->[Point]->[Color]->IO()
snowFlake w vertices [] = return ()
snowFlake w vertices (c:cs) =
		do 
		   fillTri w (invertTriangle vertices) (c)
		   sequence_ (snowFlakeList w (subTraingles vertices) cs)


main = runGraphics(
       do w<- openWindow "Snowflake" (400,400)
	  fillTri w (triangle 100 300 200) (colors!!0)
	  snowFlake w (triangle 100 300 200) (tail colors)
	  k <- getKey w
	  closeWindow w
       )

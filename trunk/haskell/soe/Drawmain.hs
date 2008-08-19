module Main
    where
import Draw
import Shape
import Graphics.HGL

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5,2.5),(-1.5,2.0),(-1.1,0.2),(-1.7,-1.0),(-3.0,0)]

shapelist = [sh1,sh2,sh3,sh4]
colors = [Red,Blue,Magenta,Yellow]
colorsShapes = zip colors shapelist

drawShapes w colorsShapes = sequence_ (map aux colorsShapes)
	where
	    aux (c,s) = drawInWindow w (withColor c (shapeToGraphics s))

main0 = 
    runGraphics(
	do w<- openWindow "Drawing Shapes" (xWin,yWin)
	   drawShapes w colorsShapes
	   spaceClose w
    )


conCircles = map circle [2.4,2.1..0.3]
coloredCircles =
    zip [Black,Blue,Green,Cyan,Red,Magenta,Yellow,White] conCircles

main=
    runGraphics(
	do w <- openWindow "Bull's Eye" (xWin, yWin)
	   drawShapes w coloredCircles
	   spaceClose w
    )



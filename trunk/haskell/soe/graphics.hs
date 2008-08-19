module Main
    where
import Graphics.HGL

{--
spaceClose::Window->IO()
spaceClose w 
      = do k <- getKey w
	   if k==' ' then closeWindow w
		     else spaceClose w
--}

main
    = runGraphics (
      do w<-openWindow
	    "My first Graphics Program" (300,300)
	 drawInWindow w (text (100,80) "Hello Graphics World\n")
	 k<-getKey w
	 drawInWindow w (withColor Red (ellipse (100, 80) (200, 180)))
	 k<-getKey w
	 closeWindow w
      )
{--
ellipse :: Point->Point->Graphics
shearEllipse :: Point->Point->Point->Graphics
line :: Point->Point->Graphics
polyline ::[Point]->Graphics
polygon :: [Point]->Graphics
polyBezier :: [Point]->Graphics
--}


-- Visualiser
-- Created by Ruben Henner Zilibowitz 18/3/2007
-- Last modified 24/3/2007

module Main where

import Data.IORef  ( IORef, newIORef, readIORef, modifyIORef, writeIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data State = State { leftMouseButton, middleMouseButton, rightMouseButton :: IORef KeyState, mouseLoc, mouseDrag :: IORef Position }

width = 800 :: Integer 
height = 600 :: Integer
cameraDistance = 40 :: GLfloat

---
--- make initial state
---
makeState :: IO State
makeState = do
   a <- newIORef Up
   b <- newIORef Up
   c <- newIORef Up
   d <- newIORef (Position 0 0)
   e <- newIORef (Position 0 0)
   return $ State { leftMouseButton = a, middleMouseButton = b, rightMouseButton = c, mouseLoc = d, mouseDrag = e }

---
--- display callback
---
display :: State -> IO ()
display state = do
   clear [ColorBuffer, DepthBuffer]
   
   materialAmbient Front $= (Color4 0.3 0.3 0.3 1.0)
   materialDiffuse Front $= (Color4 0 0 1 1)
   materialSpecular Front $= (Color4 1 1 1 1)
   materialShininess Front $= 0.8 * 128
   
   -------
   -- Commenting out the line below here causes the plane to be rendered facing towards the camera.
   -- Leaving this following line uncommented causes it to be facing away from the camera.
   -------
   preservingMatrix (do {translate (Vector3 0 0 (2::Float)); renderObject Solid (Sphere' 1 50 50)})
   renderPrimitive Quads$mapM_ (\(x,y,z)->vertex$Vertex3 x y z) [(4,0,0),(0,4,0),((-4),0,0),(0,(-4),0::Float)]
   
   swapBuffers
   postRedisplay Nothing

---
--- reshape callback
---
reshape :: Size -> IO ()
reshape size@(Size w h) = do
   let h = fromIntegral height / fromIntegral width
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   frustum (-1) 1 (-h) h 5 60
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-cameraDistance))

---
--- keyboard and mouse callback
---
keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state (Char c) Down _ _ = case c of
   'w'   -> putStrLn "nyi"
   '\27' -> exitWith ExitSuccess
   _     -> return ()
keyboardMouse state (MouseButton LeftButton) buttonState _ pos =
   do writeIORef (leftMouseButton state) buttonState
      writeIORef (mouseLoc state) pos
keyboardMouse state (MouseButton MiddleButton) buttonState _ pos =
   do writeIORef (middleMouseButton state) buttonState
      writeIORef (mouseLoc state) pos
keyboardMouse state (MouseButton RightButton) buttonState _ pos =
   do writeIORef (rightMouseButton state) buttonState
      writeIORef (mouseLoc state) pos
keyboardMouse _ _ _ _ _ = return ()

---
--- motion callback
---
motion :: State -> MotionCallback
motion state pos = do
   oldPos <- readIORef (mouseLoc state)
   writeIORef (mouseLoc state) pos
   writeIORef (mouseDrag state) ((\(Position a b) (Position x y) -> Position (a-x) (b-y)) pos oldPos)
   
   leftButton <- readIORef (leftMouseButton state)
   middleButton <- readIORef (middleMouseButton state)
   rightButton <- readIORef (rightMouseButton state)
   (Position mousex mousey) <- readIORef (mouseDrag state)
   
   case leftButton of
      Down -> do rotate (fromIntegral mousex / 10) (Vector3 0 1 (0 :: Double))
                 rotate (fromIntegral mousey / 10) (Vector3 1 0 (0 :: Double))
      Up -> putStr ""
   case middleButton of
      Down -> translate (Vector3 (fromIntegral mousex / 20)
                                 (fromIntegral mousey / 20)
                                 (0.0 :: Double))
      Up -> putStr ""
   case rightButton of
      Down -> translate (Vector3 (fromIntegral mousex / 20)
                                 (0.0 :: Double)
                                 (fromIntegral mousey / 20))
      Up -> putStr ""

---
--- main
---
main :: IO ()
main = do
   (_progName, args) <- getArgsAndInitialize
   initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
   
   initialWindowPosition $= Position 0 0
   initialWindowSize $= Size (fromInteger width) (fromInteger height)
   createWindow _progName
   
   state <- makeState
   myInit
   
   displayCallback $= display state 
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state)
   motionCallback $= Just (motion state)
   
   clearColor $= backgroundColor
   clear [ColorBuffer, DepthBuffer]
   
   mainLoop

backgroundColor = Color4 0.0 0.0 0.0 0.0

---
--- Initialize depth buffer, projection matrix, light source, and lighting
--- model. Do not specify a material property here.
---
myInit :: IO ()
myInit = do
   ambient (Light 0) $= Color4 0 0 0 1
   diffuse (Light 0) $= Color4 1 1 1 1
   position (Light 0) $= Vertex4  3 3 3 3
   lightModelAmbient $= Color4 0.2 0.2 0.2 1
   lightModelLocalViewer $= Disabled

   frontFace $= CW
   lighting $= Enabled
   light (Light 0) $= Enabled
   autoNormal $= Enabled
   normalize $= Enabled
   depthFunc $= Just Less
   
   return ()

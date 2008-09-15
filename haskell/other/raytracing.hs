module Main
    where
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

initialWindowSizeX = 800
initialWindowSizeY = 800

pixelsPerUnit = 64

main = do
    getArgsAndInitialize
    initialDisplayMode $= [RGBAMode, DoubleBuffered]
    initialWindowSize $= Size initialWindowSizeX initialWindowSizeY
    (Size screenSizeX screenSizeY) <- get screenSize
    let initialPos = Position x y where
        x = (screenSizeX - initialWindowSizeX) `div` 2
        y = (screenSizeY - initialWindowSizeY) `div` 2
    initialWindowPosition $= initialPos
    createWindow "Grid Raytracing Demo"

    endpoints <- newIORef []

    displayCallback $= display endpoints
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboard endpoints)
    matrixMode $= Projection
    
    mainLoop

display endpoints = do
    clear [ColorBuffer]
    points <- readIORef endpoints
    windowPixelSize <- get windowSize
    drawSquares points
    drawGrid windowPixelSize
    drawLine points
    swapBuffers

reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    loadIdentity
    ortho2D 0 ((fromIntegral w) / pixelsPerUnit) 0 ((fromIntegral h) / pixelsPerUnit)

keyboard _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard endpoints (MouseButton LeftButton) Down _ pos = do
    (wx, wy) <- worldFromScreen pos
    modifyIORef endpoints (addEndpoint (wx, wy))
    postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

addEndpoint pos (_:_:_) = [pos]
addEndpoint pos endPoints = pos : endPoints

drawSquares (end1 : end0 : _) = do
    currentColor $= Color4 0.25 0.25 0.5 1
    renderPrimitive Quads $ mapM_ drawQuad (squaresOnLine end0 end1)
    where
        drawQuad (x, y) = do
            vertex $ Vertex2 x0 y0
            vertex $ Vertex2 x1 y0
            vertex $ Vertex2 x1 y1
            vertex $ Vertex2 x0 y1
            where
                x0 :: GLint
                x0 = fromIntegral x
                x1 = fromIntegral x + 1
                y0 = fromIntegral y
                y1 = fromIntegral y + 1
drawSquares _ = return ()

drawGrid (Size sizeX sizeY) = do
    currentColor $= Color4 0.5 0.5 0.5 1
    renderPrimitive Lines $ mapM_ (\(x, y) -> vertex $ Vertex2 x y) points
    where
        points = (interleave minYs maxYs) ++ (interleave minXs maxXs)
        minXs = zip (repeat 0) ys
        maxXs = zip (repeat maxX) ys
        minYs = zip xs (repeat 0)
        maxYs = zip xs (repeat maxY)
        xs = take (ceiling maxX) [0..]
        ys = take (ceiling maxY) [0..]
        maxX = (fromIntegral sizeX) / pixelsPerUnit
        maxY = (fromIntegral sizeY) / pixelsPerUnit

interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave _ _ = []

drawLine ((x0, y0) : (x1, y1) : _) = do
    currentColor $= Color4 1 1 1 1
    renderPrimitive Lines $ do
        vertex $ Vertex2 x0 y0
        vertex $ Vertex2 x1 y1
drawLine ((x0, y0) : _) = do
    currentColor $= Color4 1 1 1 1
    renderPrimitive Points $ do
        vertex $ Vertex2 x0 y0
drawLine _ = return ()

worldFromScreen (Position sx sy) = do
    viewport@(_, Size _ viewSizeY) <- get viewport
    projectionMatrix <- get (matrix $ Just Projection) :: IO (GLmatrix GLdouble)
    modelviewMatrix <- get (matrix $ Just $ Modelview 0) :: IO (GLmatrix GLdouble)
    let screenPos = Vertex3 (fromIntegral sx) (fromIntegral ((viewSizeY - 1) - sy)) 0
    (Vertex3 wx wy wz) <- unProject screenPos projectionMatrix modelviewMatrix viewport
    return (wx, wy)

squaresOnLine (x0, y0) (x1, y1) = take n (genSquares (floor x0) (floor y0) error) where
    n = 1 + abs((floor x1) - (floor x0)) + abs((floor y1) - (floor y0))
    dx = abs (x1 - x0)
    dy = abs (y1 - y0)
    xInc = if x1 > x0 then 1 else -1
    yInc = if y1 > y0 then 1 else -1
    error = dy * tNextX - dx * tNextY
    tNextX
        | x1 > x0   = (fromIntegral (ceiling x0)) - x0
        | otherwise = x0 - (fromIntegral (floor x0))
    tNextY
        | y1 > y0   = (fromIntegral (ceiling y0)) - y0
        | otherwise = y0 - (fromIntegral (floor y0))
    genSquares x y error
        | error > 0 = (x, y) : genSquares x (y + yInc) (error - dx)
        | otherwise = (x, y) : genSquares (x + xInc) y (error + dy)

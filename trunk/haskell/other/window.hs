module Main where

import Graphics.HGL

main :: IO ()
main = runGraphics $
    withWindow_ "Hello World Window" (300, 200) $ \ w -> do
    drawInWindow w $ text (100, 100) "Hello World"
    drawInWindow w $ ellipse (100, 80) (200, 180)
    getKey w

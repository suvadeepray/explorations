module Main 
    where

import System.Environment

main::IO ()

main = do putStrLn "Enter your name: "
          line <- getLine
          putStrLn("Hello " ++ line)
       

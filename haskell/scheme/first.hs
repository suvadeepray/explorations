module Main where
import System.Environment

main::IO()

addSpace xs = " " ++ xs

{--
main = do args <- getArgs
	  putStrLn (foldl (++) [] (["Hello, "] ++  (map addSpace args)) )
--}

operation s 
    | s=="+" = (+)
    | s=="-" = (-)
    | s=="*" = (*)
    | s=="/" = (/)
    | s=="**" = (**)

main = do args <- getArgs
	  putStrLn $ show $ (operation (args!!0)) (read (args!!1)) (read (args!!2))

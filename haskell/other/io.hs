module Io
    where

import IO
import Control.Monad

f::Int->IO()
{--
f x | x==0  = return()
    | otherwise = do putStr "Recursing"
		     f (x-1)
--}

{--
f x =
    if x == 0
    then do putStr "End of recursion"
    else do putStr "Recursing"
	    f (x-1)
--}

f x = when (not (x==0)) $ do 
    putStr "Recursing"
    f (x-1)


copy infile outfile = do hin <- openFile infile ReadMode
                         hout <- openFile outfile WriteMode
                         filedata <- hGetContents hin
                         hPutStr hout filedata
                         hClose hout
                         putStrLn "Finished copying"
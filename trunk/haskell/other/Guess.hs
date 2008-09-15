module Main
    where

import IO
import Random

{--
main = do
    hSetBuffering stdin LineBuffering
    num <- randomRIO (1::Int,100)
    putStrLn "I am thinking of a number between 1 and 100"
    doGuessing num

doGuessing num = do
    putStrLn "Enter your guess: "
    guess <- getLine
    let guessNum = read guess
    if guessNum < num
	then do putStrLn "Too low!"
		doGuessing num
	else if read guess > num
	    then do putStrLn "Too high!"
		    doGuessing num
	    else do putStrLn "You win!"
	
--}

getString (x:xs)=
    if length xs == 0
	then x
	else x ++ " " ++  getString xs

printString l = putStrLn (getString l)
    
	
main = do
    list <- askForWords
    printString list

askForWords = do
    putStrLn "Enter a word: "
    word <- getLine
    if word==""
	then return []
	else do wordList <- askForWords
		return (word:wordList)

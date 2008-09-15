module Main
    where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol::Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces::Parser ()
spaces = skipMany1 space


readExpr::String->String
readExpr input = case parse (space>>symbol) "lisp" input of
    Left err -> "No match found: " ++ show err 
    Right val-> "Found value"

data LispVal = Atom String
	    | List [LispVal]
	    | DottedList [LispVal] LispVal
	    | Number Integer
	    | String String
	    | Bool Bool

parseString::Parser LispVal
parseString = do char '"'
		 x <- many (noneOf "\"")
		 char '"'
		 return $ String x

main::IO ()
main = do args <- getArgs
	  putStrLn (readExpr (args!!0))

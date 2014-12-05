module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
{--

    > :load 02_parsing/00_write_a_simple_parser.hs
    
    compile: ghc -o srcfiles/02_simple_parser 02_parsing/00_write_a_simple_parser.hs
    run:     srcfiles/02_simple_parser "\"coffee\""
--}

-- step 1: parse a char.
-- step 2: parse a char preceded by a one or more spaces.
-- step 3: we want something more out of our parsers: we want them to convert the input into a data structure that we can traverse easily.
--        define a data type, and how to modify our parser so that it returns this data type.
--        a). define a parser that recognizes any number of whitespace characters
--        b). A string is a double quote mark, followed by any number of non-quote characters, followed by a closing quote mark:        

--  the "extra information" that is being hidden is all the info about position in the 
--  input stream, backtracking record, first and follow sets, etc.
--  type Parser = Parsec Text.Text ()
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- spaces :: Parser Char
-- spaces =  char "'"
spaces :: Parser ()
spaces = skipMany1 space

-- define a data type that can hold any Lisp value
data LispVal = Atom String 
             | String String
             | Number Integer 
             | Bool Bool  
             | List [LispVal]
             | DottedList [LispVal] LispVal

-- print helpers / patten matching on typed constructors.
showVal :: LispVal -> String
showVal (String str) =  "[\"" ++ str ++ "\"]"
showVal (Atom str) =    "[" ++ str ++ "]"
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (Number num) =  "[" ++ (show num) ++"]"

instance Show LispVal where show = showVal

-- parsers to support the LispVal data types
-- A string is a double quote mark, followed by any number of non-quote characters, followed by a closing quote mark:
-- many :: ParsecT s u m a -> ParsecT s u m [a]
-- Couldn't match expected type `Text.Parsec.Prim.ParsecT s0 u0 m0 a0'
--             with actual type `[Char] -> Text.Parsec.Prim.ParsecT s1 u1 m1 Char'
-- parseString
parseString :: Parser LispVal
parseString = do
                char '"'
                s <- many (noneOf("\""))
                char '"'
                return $ String s

-- An atom is a letter or symbol, followed by any number of letters, digits, or symbols: 
parseAtom :: Parser LispVal
parseAtom = do
             first <- (letter <|> symbol)
             rest <- many(letter <|> digit <|> symbol)
             let atom = first : rest
             return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _ -> Atom atom

-- Finally, we create one more parser, for numbers. This shows one more way of dealing with monadic values:
parserNumbers :: Parser LispVal

-- do sugar
-- parserNumbers = do 
--                  numStr <- many digit
--                  return $ Number(read numStr)

-- bind >>=
-- parserNumbers = (many digit) >>= \numStr -> return $ Number(read numStr)

-- liftM and Number composed with read. read as defined on Integer:
-- instance Read Integer, read :: String -> Integer
--  Number(Integer) when composed Number(String -> Integer)
parserNumbers = liftM (Number . read) $ many1 digit

-- Let's create a parser that accepts either a string, a number, or an atom:
parserExpr :: Parser LispVal
parserExpr = parseString 
            <|> parserNumbers 
            <|> parseAtom

readExpr :: String -> String
readExpr input = case parse (parserExpr) "(LISP)" input of
                    Left err -> "Didn't find value " ++ show err
                    Right val -> "Found value " ++ show val


main :: IO()
main = do
        args <- getArgs
        putStrLn (readExpr ( args !! 0) )

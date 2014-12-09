module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.IORef
import System.IO.Unsafe
-- 
test :: IORef [a]
test = unsafePerformIO $ newIORef []

counter :: IORef Integer
counter = unsafePerformIO $ newIORef 0    
{--

    Our strings aren't quite R5RS compliant, because they don't support escaping of 
    internal quotes within the string. Change parseString so that \" gives a literal 
    quote character instead of terminating the string. You may want to replace noneOf "\"" 
    with a new parser action that accepts either a non-quote character or a backslash 
    followed by a quote mark.
    
    compile: ghc -o srcfiles/02_02_exercise 02_parsing/02_02_exercise.hs
    run:     srcfiles/02_02_exercise "\"coffee \\" internal text \\" \""
    
--}

data LispVal = String String

showVal :: LispVal -> String
showVal (String str) = "\"" ++ str ++ "\""

instance Show LispVal where show = showVal

-- need unsafe io.
-- System.IO.Unsafe 
parserEscapeChar :: Parser Char
parserEscapeChar = do
                    first <- char '\\'
                    next <- oneOf "\\\""
                    return next
                    
parserEscapeQuotes :: Parser LispVal
parserEscapeQuotes = do
                      char '"'
                      str <- many(parserEscapeChar <|> noneOf("\"\\"))  -- either backslash or '"'
                      char '"'
                      return $ String(str)

readExprs :: String -> String
readExprs input = case parse (parserEscapeQuotes) "(source)" input of
                    Left err  -> "Not found" ++ show err
                    Right val -> "Found value " ++ show val

main :: IO()
main = do
        args <- getArgs 
        putStrLn(readExprs(args !! 0) )
        
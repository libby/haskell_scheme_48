module Main where
import System.Environment
import Text.ParserCombinators.Parsec

{--

    7. Add data types and parsers to support the full numeric tower of Scheme numeric types. 
        Haskell has built-in types to represent many of these; check the Prelude. For the others,
        you can define compound types that represent eg. a Rational as a numerator and denominator, 
        or a Complex as a real and imaginary part (each itself a Real).
        
    TODO:
--}

newtype LispVal = Numeric Integer
 deriving Show

readExprs :: String -> String
readExprs input = case parse RULE "(Lisp)" input of
                    Left err -> "Not Found " ++ show err
                    Right val -> "Found val " ++ show val

main :: IO()
main = do
        args <- getArgs
        putStrLn args !! 0
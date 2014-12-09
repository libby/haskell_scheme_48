module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

{--

    Rewrite parseNumber, without liftM, using

        do-notation
        explicit sequencing with the >>= operator
        
    compile: ghc -o srcfiles/02_01_exercise 02_parsing/02_01_exercise.hs
    run:     srcfiles/02_01_exercise "2828"
--}

data LispVal = Number Integer 

showVal :: LispVal -> String
showVal (Number num) = "[" ++ (show num) ++ "]"

instance Show LispVal where show = showVal

parserNumbers :: Parser LispVal
parserNumbers = (many digit) >>= \numStr -> return $ Number(read numStr)

readExprs :: String -> String
readExprs input = case parse (parserNumbers) "(source)" input of
                    Left err  -> "Not found " ++ show err
                    Right val -> "Found Val" ++ show val 

main :: IO()
main = do
        arg <- getArgs
        putStrLn(readExprs (arg !! 0))
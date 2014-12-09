module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import Numeric (readFloat)

{--

    5. Add a Float constructor to LispVal, and support R5RS syntax for decimals. The Haskell function readFloat may be useful.
       https://www.haskell.org/onlinereport/numeric.html#sect14 // readFloat
       http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
       
   compile: ghc -o srcfiles/02_06_exercise_R5RS_float 02_parsing/02_06_exercise_R5RS_float.hs
   run:     srcfiles/02_06_exercise_R5RS_float "1.76549"
   run:     srcfiles/02_06_exercise_R5RS_float ".76549"
   
--}

newtype LispVal = Float Float
 deriving Show
 
stringToFloat x = fst $ readFloat x !! 0

parserFloatDecimalOnly :: Parser LispVal
parserFloatDecimalOnly = do
                          char '.'
                          dec <- many(digit)
                          return $ Float . stringToFloat $ '0' : '.' : dec

parserFloat :: Parser LispVal
parserFloat = do
                first <- many1(digit) 
                char '.'
                decimal <- many(digit)
                let floatStr = first ++ '.' : decimal
                return $ (Float . stringToFloat) floatStr

readExprs :: String -> String
readExprs input = case parse (parserFloat <|> parserFloatDecimalOnly) "(LISP)" input of
                    Left err -> "Not Found " ++ show err
                    Right val -> "Found " ++ show val
                     
main :: IO()
main = do
        args <- getArgs
        putStrLn(readExprs (args !! 0) )
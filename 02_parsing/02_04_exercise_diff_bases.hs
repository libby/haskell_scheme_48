module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char (digitToInt)
import Numeric

{--

    Exercise 4. 
    
    Change parseNumber to support the Scheme standard for different bases. 
    You may find the readOct and readHex functions useful.    
    
    http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
    A number may be written in binary, octal, decimal, or hexadecimal by the use of a radix prefix. 
    The radix prefixes are #b (binary), #o (octal), #d (decimal), and #x (hexadecimal). With no radix prefix, 
    a number is assumed to be expressed in decimal.
    
    compile: ghc -o srcfiles/02_04_exercise_diff_bases 02_parsing/02_04_exercise_diff_bases.hs
    run:     srcfiles/02_04_exercise_diff_bases "123"
    run:     srcfiles/02_04_exercise_diff_bases "#b1000"
    run:     srcfiles/02_04_exercise_diff_bases "#x1000"
    run:     srcfiles/02_04_exercise_diff_bases "#o1000"
    
--}

data LispVal = Number Integer

showVal :: LispVal -> String
showVal (Number num) = show num

instance Show LispVal where show = showVal

-- #b (binary), #o (octal), #d (decimal), and #x (hexadecimal).
-- paserRadix :: Parser LispVal
-- paserRadix = do
--               x <- char '#' 
--               r <- oneOf "bodx"
--               return (Number . read $ 
--                 case r of
--                 'b' -> many1 digit  -- hexadecimal
--                 'o' -> many1 digit --  octal
--                 'd' -> many1 digit  -- decimal
--                 'x' -> many1 digit -- hexadecimal
--                 )

-- binary2Digit :: Integer -> Integer
-- binary2Digit binNum = 
hex2Digit x = fst $ readHex x !! 0
oct2Digit x = fst $ readOct x !! 0

-- readInt :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
-- readInt base isDigit valDigit
-- readInt 2 (\x -> (x == '1') || (x == '0')) (\c -> 1)
-- digitToInt :: Char -> Int
-- readInt 2 (\x -> (x == '1') || (x == '0')) digitToInt
-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
-- readInt 2 (`elem` "01") digitToInt
readBinary num = (readInt 2 (\x -> (x == '1') || (x == '0')) digitToInt) num
bin2Digit x = fst $ readBinary x!! 0

parseBinary :: Parser LispVal
parseBinary = do try $ string "#b"
                 x <- many1 (oneOf "01")
                 return $ Number (bin2Digit x)
                
parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return  $ Number (hex2Digit x)

{--

    When consuming other bases, you must try the whole combination of '#' plus <base_token>, i.e. `string "#o"`
    if you were to try and consume the '#' and then the token,
    ` do char '#'
        char 'o' <> char 'O'
      ...    
    `    
    Then then parser would match the '#' for any base and would there for not be useable in a <|> chain
    parseOct <|> parseBinary  -- parseBinary would not be reachable as parseOct would always match '#' and, in the 
                                case of a following 'b' fail, however  the rules of <|> are if there is an 
                                initial match then that parser is selected.

--}
parseOct :: Parser LispVal
parseOct = do try $ (char '#' >> (char 'o' <|> char 'O')) -- accept either upper or lowercase 'o'                   
              x <- many1 octDigit
              return $ Number (oct2Digit x)
            
parseDigit :: Parser LispVal
parseDigit = do
                x <- many1 digit
                return $ (Number . read $ x)
    
paserNumber :: Parser LispVal
paserNumber = parseDigit  <|> parseHex <|> parseOct <|> parseBinary

readExprs :: String -> String
readExprs input = case parse  paserNumber "(Lispval)" input of
                    Left err    -> "Not Found " ++ show err
                    Right val   -> "Found val " ++ show val

main :: IO()
main = do
        args <- getArgs
        putStrLn(readExprs (args !! 0))
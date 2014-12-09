module Main where
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct, readInt)
import Data.Char (digitToInt)
import Control.Monad (liftM)
{--
    
    Recursive Parsers: Adding lists, dotted lists, and quoted datums
    
    1. Next, we add a few more parser actions to our interpreter. Start with the parenthesized lists that make Lisp famous:
    
    2. The dotted-list parser is somewhat more complex, but still uses only concepts that we're already familiar with:
    
    3. let's add support for the single-quote syntactic sugar of Scheme:
    
    4.  edit our definition of parseExpr to include our new parsers:

    > compile:  ghc -o srcfiles/01_simple_parser_with_lists 02_parsing/01_simple_parser_with_lists.hs
    > run:      srcfiles/01_simple_parser_with_lists "\"coffee\""
    >           srcfiles/01_simple_parser_with_lists " \"coff\\\"ee\""
    >           srcfiles/01_simple_parser_with_lists " \"coff\\tee\""
    >           srcfiles/01_simple_parser_with_lists " \"coff\\\"\tee\""
    >           srcfiles/01_simple_parser_with_lists "#t"
    >           srcfiles/01_simple_parser_with_lists "1.234"
    >           srcfiles/01_simple_parser_with_lists "#b1000" 
    >           srcfiles/01_simple_parser_with_lists "#x1000"
    >           srcfiles/01_simple_parser_with_lists "#o1000"
    >           srcfiles/01_simple_parser_with_lists "(#o1000 #x1000 #b1000)"
    --  ‘(1 . (2 . (3 . nil)))’.    
    >           srcfiles/01_simple_parser_with_lists "(#o1000 . (#x1000 . (#b1000)))"
    >           srcfiles/01_simple_parser_with_lists "(1 . (2 . (3 . nil)))"
    
    
    
--}

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal = String String  |
               Atom String    |
               List [LispVal] |
               DottedList [LispVal] LispVal |
               Number Integer |
               Bool Bool      |
               Character Char |
               Float Float
               
showVal :: LispVal -> String
showVal (String s) = "[\"" ++ s ++ "\"]"
showVal (Atom   s) = "Atom: [" ++ s ++ "]"
showVal (Number x) = "Number: [" ++ show x ++ "]"
-- showVal (Bool b)   = case b of 
--                         True -> "True"
--                         False -> "False"
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (Character c) = "Char: [" ++ [c] ++ "]"
showVal (Float f) = "Float: [" ++ show f ++ "]"
showVal (List xs) = "List: [" ++ show xs ++ "]"
showVal (DottedList xs x) = "DottedList: [" ++ show xs ++ show x ++ "]"

instance Show LispVal where show = showVal

spaces :: Parser ()
spaces = skipMany space

lowercase :: Parser Char
lowercase = oneOf ['a'..'z']
uppercase = oneOf ['A'..'Z']
leftParen = char '('
parseNewline = try $ do 
                 string "newline"
                 return '\n'
parseSpace = try $ do
                    (string " " <|> string "space")
                    return ' '
                    
-- Character literals are #\space, #\a, #\A, #\(, #\newline 
parserCharLiteral :: Parser LispVal
parserCharLiteral = try $ do
                            string "#\\"
                            c <- (parseNewline <|> parseSpace <|> lowercase <|> uppercase <|> leftParen)
                            return $ Character c
                            
                     
-- quotes " this is an \" escaped quote \" "
escapedQuote :: Parser Char
escapedQuote = do
                string "\\\"" 
                return '"'

-- escaped chars \t \r \n
specialChars :: Parser Char
specialChars = do
                char '\\'
                c <- oneOf "trn"
                return $ case c of
                            't' -> '\t'
                            'r' -> '\r'
                            'n' -> '\n'
                            _ -> c

parserAtom :: Parser LispVal        
parserAtom = do
              first <- (letter <|> symbol)
              rest  <- many1(letter <|> digit <|> symbol)
              let atom = [first] ++ rest
              return $ Atom atom

parserBool :: Parser LispVal
parserBool = do 
              b <- (char '#' >> oneOf "tf")
              return $ Bool $ case b of 
                                't' -> True
                                'f' -> False

parserString :: Parser LispVal
parserString = do
                spaces
                char '"'
                spaces
                first <- letter
                rest  <- many1(try(specialChars) <|> digit <|> letter <|> escapedQuote)
                char '"'
                let str = [first] ++ rest
                return $ String str

parserDec :: Parser LispVal
parserDec = do
              d <- many1 digit
              return $ (Number . read) $ d

-- support hexidecimal if prefixed with #h
hexToDigit x = fst $ (readHex x) !! 0
parserHex :: Parser LispVal
parserHex = try $ do
                    x <- string "#x" >> many1 digit
                    return $ Number . hexToDigit $ x

-- support octal if prefixed with #o or #O
octToDigit x = fst $ readOct x !! 0
parserOct :: Parser LispVal
parserOct = try $ do
                    char '#'
                    (char 'o' <|> char 'O')
                    x <- many1 digit
                    return $ Number . octToDigit $ x

-- readInt :: Num a
--   => a                  -- ^ the base
--   -> (Char -> Bool)     -- ^ a predicate distinguishing valid digits in this base
--   -> (Char -> Int)      -- ^ a function converting a valid digit character to an 'Int'
--   -> ReadS a
binaryToDigit num = readInt 2 (\x -> (x == '0' || x == '1')) (digitToInt) num
readBinary x = fst $ binaryToDigit x !! 0
 
parserBinary :: Parser LispVal
parserBinary = try $ do
                      char '#'
                      (char 'b' <|> char 'B')
                      x <- many1 digit
                      return $ Number . readBinary $ x
                      
-- Support Floating point numbers both with and without whole number digit, i.e. .123 and 1.23
toFloat x = fst $ readFloat x !! 0
parserFloat :: Parser LispVal
parserFloat = do
                w <- many1 digit 
                char '.'
                d <- many1 digit
                let floarstr = w ++ ['.'] ++ d
                return $ (Float . toFloat) $ floarstr

-- a number is either base 2, 8, 10 or 16: binary, octal, decimal, or hex
parserNumber :: Parser LispVal
parserNumber = parserDec <|> parserOct <|> parserHex <|> parserBinary

-- Next, we add a few more parser actions to our interpreter. Start with the parenthesized lists that make Lisp famous:    
parserList :: Parser LispVal
parserList = liftM List $ sepBy parserExpr spaces

--  ‘(1 . (2 . (3 . nil)))’.
-- (3 . nil)
-- parserParenThenNum :: Parser Lispval
-- parserParenThenNum = do
--                         leftSide <- (char '(' >> (parserNumber <|> parserString) >> space >> char '.')
--                         rightParen <- many1 char '))'
-- size leftSide == size right paren
parserDottedList :: Parser LispVal
parserDottedList = do
                    car <- endBy parserExpr spaces
                    cdr <- char '.' >> spaces >> parserExpr
                    return $ DottedList car cdr
-- Next, let's add support for the single-quote syntactic sugar of Scheme:
parserSingleQuote :: Parser LispVal
parserSingleQuote = try $ do
                           qStart <- char '\''
                           x <- parserExpr
                           return $ List [Atom "quote", x]
    
-- parserList = do
--               char '('
--               xs <- many parserExpr
--               char ')'
--               return $ List xs

parserExpr :: Parser LispVal
parserExpr =  try(parserFloat) 
                <|> parserString 
                <|> parserNumber 
                <|> parserCharLiteral 
                <|> parserBool 
                <|> parserAtom 
                <|> parserSingleQuote               
                <|> do char '('
                       xs <- try parserList <|> parserDottedList
                       char ')'
                       return xs

readExpr :: String -> String
readExpr input = case parse (parserExpr) "(Lisp)" input of
                    Left err  -> "Not Found " ++ show err
                    Right val -> "Found "     ++ show val
    
main :: IO()
main = do
        args <- getArgs
        putStrLn(readExpr (args !! 0))
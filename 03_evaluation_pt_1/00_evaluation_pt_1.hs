module Main where
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct, readInt)
import Data.Char (digitToInt)
import Control.Monad (liftM)
--import Prelude.unword

{--
    
    Recursive Parsers: Adding lists, dotted lists, and quoted datums
    
    1. Next, we add a few more parser actions to our interpreter. Start with the parenthesized lists that make Lisp famous:
    
    2. The dotted-list parser is somewhat more complex, but still uses only concepts that we're already familiar with:
    
    3. let's add support for the single-quote syntactic sugar of Scheme:
    
    4.  edit our definition of parseExpr to include our new parsers:

    > compile:  ghc -o srcfiles/eval_1 03_evaluation_pt_1/00_evaluation_pt_1.hs
    > run:      srcfiles/eval_1 "'atom" 
    >           srcfiles/eval_1 "1.234" 
    >           srcfiles/eval_1 "\"a string\"" 
    >           srcfiles/eval_1 "'atom"
    >           srcfiles/eval_1 "(+ 2 3)" 
    >           srcfiles/eval_1 "(* 5 3)" 

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
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom   s) = s
showVal (Number x) = show x
-- showVal (Bool b)   = case b of 
--                         True -> "True"
--                         False -> "False"
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (Character c) = "\'" ++ [c] ++ "\'"
showVal (Float f) = show f
showVal (List xs) = "List: [" ++ show xs ++ "]"
showVal (DottedList xs x) = "DottedList: [" ++ show xs ++ show x ++ "]"

instance Show LispVal where show = showVal

-- eval
unwordsList :: [LispVal] -> String
-- see 
-- :t Prelude.unwords  [String] -> String,
-- :t map (a -> b) -> [a] -> b
--  point-free style: writing definitions purely in terms of function composition and partial application
unwordsList = unwords . map showVal

-- takes a primitive Haskell function (often an operator section) and wraps it 
-- with code to unpack an argument list, apply the function to it, and wrap the result up in our Number constructor.
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n

unpackNum (String str) = let parsed = read str :: [(Integer, String)] in
                            if null parsed
                                then 0
                                else fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

-- Adding basic primitives
primatives :: [(String, [LispVal] -> LispVal)]
primatives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem)
             ]

apply :: String -> [LispVal] -> LispVal
apply fn args = maybe (Bool False) ($ args) $ lookup fn primatives             





-- Evaluating numbers, strings, booleans, and quoted lists is fairly simple: return the datum itself.
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Atom _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
-- Quoted List
-- TODO: confused on this type
eval (List ([Atom "quote", val])) = val
-- We have to recursively evaluate each argument, so we map eval over the args. 
-- This is what lets us write compound expressions like (+ 2 (- 3 1) (* 5 4)). 
-- Then we take the resulting list of evaluated arguments, and pass it and the original function to apply:
eval (List (Atom fn : args)) = apply fn $ map eval args


-- parsing

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
              rest  <- many(letter <|> digit <|> symbol)
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
                rest  <- many(try(specialChars) <|> digit <|> letter <|> escapedQuote <|> space) 
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
parserFloat = try $ do
                      first <- many digit
                      char '.'
                      d <- many1 digit
                      let floarstr = case first of
                                        [] -> '0' : ['.'] ++ d
                                        _ -> first ++ ['.'] ++ d
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
parserExpr =    parserFloat
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

readExpr :: String -> LispVal
readExpr input = case parse (parserExpr) "(Lisp)" input of
                    Left err  -> String $ "Not Found " ++ show err
                    Right val -> val
    
-- main :: IO()
-- main = do
--         args <- getArgs
--         putStrLn(readExpr (args !! 0))
main :: IO()
main = getArgs >>= print . eval . readExpr . head
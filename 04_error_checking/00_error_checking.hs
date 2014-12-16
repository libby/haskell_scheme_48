module Main where
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct, readInt)
import Data.Char (digitToInt)
import Control.Monad (liftM)
import Control.Monad.Error -- is deprecated: Use Control.Monad.Except instead

{--
    
    Recursive Parsers: Adding lists, dotted lists, and quoted datums
    
    1. Next, we add a few more parser actions to our interpreter. Start with the parenthesized lists that make Lisp famous:
    
    2. The dotted-list parser is somewhat more complex, but still uses only concepts that we're already familiar with:
    
    3. let's add support for the single-quote syntactic sugar of Scheme:
    
    4.  edit our definition of parseExpr to include our new parsers:

    > compile:  ghc -o srcfiles/error_check 04_error_checking/00_error_checking.hs
    > run:      srcfiles/error_check "(+ 2 \"two\")"
    >           srcfiles/error_check  "(+ 2)"
    >           srcfiles/error_check "(what? 2)"

--}

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal = String String
               | Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | Bool Bool
               | Character Char
               | Float Float
               
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

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = "[" ++ message ++ " : " ++  varname ++ "]"
showError (BadSpecialForm message form) = "[" ++ message ++ " : " ++ show form ++ "]"
showError (NotFunction message func) = "[" ++ message ++ " : " ++ show func ++ "]"
showError (NumArgs expected found) = "[ Expected : " ++ show expected ++ ", found " ++ show found ++ "]"
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
 
instance Show LispError where 
    show = showError
    
instance Error LispError where 
    noMsg = Default "An error has occurred"
    strMsg = Default

-- represents a Type that may throw an Error
-- Type constructors are curried just like functions, and can also be partially applied.
type ThrowsError = Either LispError

-- convert all errors to a String representation
-- "catchError, which takes an Either action and a function that turns an error into another Either action. 
--  If the action represents an error, it applies the function, which you can use to, e.g. turn the 
--  error value into a normal one via return or re-throw as a different error.""
trapError action = catchError action (return . show)

--     
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Evaluating numbers, strings, booleans, and quoted lists is fairly simple: return the datum itself.
eval :: LispVal -> ThrowsError LispVal
eval val@(String _)  = return val
eval val@(Atom _)    = return val
eval val@(Number _)  = return val
eval val@(Bool _)    = return val
eval val@(Float _)   = return val
-- Quoted List
-- TODO: confused on this type
eval (List ([Atom "quote", val])) = return val
-- We have to recursively evaluate each argument, so we map eval over the args. 
-- This is what lets us write compound expressions like (+ 2 (- 3 1) (* 5 4)). 
-- Then we take the resulting list of evaluated arguments, and pass it and the original function to apply:
--eval (List (Atom fn : args)) = apply fn $ map eval args
-- Now that wrapped in Monadic ThrowsError
eval (List (Atom fn : args)) = mapM eval args >>= apply fn

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- applying functions

apply :: String -> [LispVal] -> ThrowsError LispVal
-- apply fn args = maybe (Bool False) ($ args) $ lookup fn primatives             
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function args" fn) 
                      ($ args) 
                      (lookup fn primatives)             

unwordsList :: [LispVal] -> String

-- see 
-- :t Prelude.unwords  [String] -> String,
-- :t map (a -> b) -> [a] -> b
--  point-free style: writing definitions purely in terms of function composition and partial application
unwordsList = unwords . map showVal

-- takes a primitive Haskell function (often an operator section) and wraps it 
-- with code to unpack an argument list, apply the function to it, and wrap the result up in our Number constructor.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n

unpackNum (String str) = let parsed = reads str in
                                        if null parsed
                                            then throwError $ TypeMismatch "number" $ String str
                                            else return $ fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

--unpackNum _ = 0

-- Adding basic primitives
primatives :: [(String, [LispVal] -> ThrowsError LispVal)]
primatives = numbericPrimatives ++ typeCheckPrimatives

numbericPrimatives :: [(String, [LispVal] -> ThrowsError LispVal)]
numbericPrimatives = [("+", numericBinOp (+)),
                        ("-", numericBinOp (-)),
                        ("*", numericBinOp (*)),
                        ("/", numericBinOp div),
                        ("mod", numericBinOp mod),
                        ("quotient", numericBinOp quot),
                        ("remainder", numericBinOp rem)
                     ]


numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op           []  = throwError $ NumArgs 2 []
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params        = mapM unpackNum params >>= return . Number . foldl1 op

--numericBinOp op params = Number $ foldl1 op $ map unpackNum params


typeCheckPrimatives :: [(String, [LispVal] -> ThrowsError LispVal)]
typeCheckPrimatives = [
                        ("symbol?", unaryOp symbolp),
                        ("string?", unaryOp stringp),
                        ("number?", unaryOp numberp),
                        ("boolean?", unaryOp booleanp)
                       ]
                       
unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp fn [v] = fn v

symbolp, stringp, numberp, booleanp :: LispVal -> ThrowsError LispVal
symbolp (Atom _) = return $ Bool True
symbolp val@(_) = throwError $ TypeMismatch "Atom" val 
stringp (String _) = return $ Bool True
stringp val@(_) = throwError $ TypeMismatch "String" val 
numberp (Number _) = return $ Bool True
numberp val@(_) = throwError $ TypeMismatch "number" val 
booleanp (Bool _) = return $ Bool True
booleanp val@(_) = throwError $ TypeMismatch "Boolean" val 

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
-- ? Let's change it so that it wraps and throws the original ParseError:
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (parserExpr) "(Lisp)" input of
                    Left err  -> throwError $ Parser err
                    Right val -> return val
    
-- main :: IO()
-- main = do
--         args <- getArgs
--         putStrLn(readExpr (args !! 0))
-- main :: IO()
-- main = getArgs >>= print . eval . readExpr . head

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
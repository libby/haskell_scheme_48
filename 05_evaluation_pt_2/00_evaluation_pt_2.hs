{-# LANGUAGE ExistentialQuantification #-}
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
    -- -fglasgow-exts
    ghc -package parsec -fglasgow-exts -o srcfiles/eval_2  05_evaluation_pt_2/00_evaluation_pt_2.hs
    
    > compile:  ghc -o srcfiles/eval_2 05_evaluation_pt_2/00_evaluation_pt_2.hs
    > run:      srcfiles/eval_2  "(< 2 3)"
    >           srcfiles/eval_2  "(> 2 3)"
    >           srcfiles/eval_2  "(>= 3 3)"
    >           srcfiles/eval_2 "(if (> 2 3) \"no\" \"yes\")"
    >           srcfiles/eval_2 "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"
    -- with exitential types
    >           srcfiles/eval_2  "(cdr '(a simple test))"
    >           srcfiles/eval_2 "(car (cdr '(a simple test)))"
    >           srcfiles/eval_2 "(car '((this is) a test))"
    >           srcfiles/eval_2 "(cons '(this is) '())"
    >           srcfiles/eval_2 "(eqv? 1 3)"
    >           srcfiles/eval_2 "(eqv? 3 3)"
    >           srcfiles/eval_2 "(eqv? 'atom 'atom)"

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
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "\'" ++ [c] ++ "\'"
showVal (Float f) = show f
showVal (List xs) = "(" ++ show xs ++ ")"
showVal (DottedList xs x) = "DottedList: [" ++ show xs ++ show x ++ "]"

instance Show LispVal where show = showVal

-- data type and instances for handling errors that may occur when interpreting a Lisp program.
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
-- four elem list pattern match
eval (List ([Atom "if", pred, conseq, alt])) = do
                                                  result <- eval pred
                                                  case result of
                                                      Bool False -> eval alt
                                                      otherwise -> eval conseq

-- We have to recursively evaluate each argument, so we map eval over the args. 
-- This is what lets us write compound expressions like (+ 2 (- 3 1) (* 5 4)). 
-- Then we take the resulting list of evaluated arguments, and pass it and the original function to apply:
--eval (List (Atom fn : args)) = apply fn $ map eval args
-- Now that wrapped in Monadic ThrowsError
eval (List (Atom fn : args)) = mapM eval args >>= apply fn

-- catch all eval
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- listPrimatives car cdr
car :: [LispVal] -> ThrowsError LispVal
car [List(x : xs)] = return x
car [DottedList (x : xs) _ ] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- cdr rest of list
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x ] = return $ x
cdr [DottedList (_:xs) x ] = return $ DottedList xs x

cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- cons (cons (1 2 3) (4 5 6))
cons :: [LispVal] -> ThrowsError LispVal
-- cons List([x:xs]) List([y:ys]) = return $ List (x:xs ++ y:ys)
-- cons with the empty list Nil: LispVal 'List' containing the [] (empty list)
cons [xs, List []] = return $ List [xs]
-- cons anythign and a list
cons [someVal, List xs] = return $ List $ someVal : xs
-- with DottedLists..
-- 
cons [x, DottedList xs second] = return $ DottedList (x:xs) second
-- If you cons together two non-lists, or put a list in front, you get a DottedList. 
-- This is because such a cons cell isn't terminated by the normal Nil that most lists are.
cons [x1, x2] = return $ DottedList [x1] x2
-- attempting to cons together more or less than two arguments is an error:
cons badArgList = throwError $ NumArgs 2 badArgList

--cons [List([x:xs]), List([y:ys])] = return $ List (x:xs ++ y:ys)

-- eq?, eqv?, and equal?
eqv :: [LispVal] -> ThrowsError LispVal
--eqv [(Bool True) : (Bool False)] = return $ LispVal Bool True
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ (arg1 == arg2)
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
-- -- equivalent if the concat of their two list are eqv
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair $ zip arg1 arg2) where
                                                    eqvPair (x, y) = case eqv[x,y] of       -- eqvPair is an example of a local definition
                                                                        Left err -> False
                                                                        Right (Bool b) -> b 
-- ??                                                                        
eqv [_, _] = return $ Bool False 
eqv badArgList = throwError $ NumArgs 2 badArgList
                                                    
-- applying functions

apply :: String -> [LispVal] -> ThrowsError LispVal
-- apply fn args = maybe (Bool False) ($ args) $ lookup fn primatives             
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function args" fn) 
                      ($ args) 
                      (lookup fn primatives)             

-- equal? and Weak Typing: Heterogenous Lists. Unpack values if any of them  if any of them result in Haskell values that are equal, return True.
-- NO: unpackFunctions = [unpackNum, unpackBool, unpackString]
--equal [arg1, arg2] = mapM arg1 arg2 unpackNum

-- "For any type that is an instance of Eq, you can define an Unpacker that takes a function from LispVal to that type, and may throw an error"
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)    

-- helper function takes two LispVal and an Unpacker and determines whether the unpacked values are equal
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
                                                  unpacked1 <- unpacker arg1
                                                  unpacked2 <- unpacker arg2
                                                  return $ unpacked1 == unpacked2
                                        -- using the const function because catchError expects a function to apply to the error value.          
                                        `catchError` (const $ return False)     
unpackFunctions = [AnyUnpacker unpackNum, AnyUnpacker unpackBool, AnyUnpacker unpackString]
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
                        -- (unpackEquals arg1 arg2) partially applied function so LispVal -> LispVal => fn unpacker -> ThrowsError Bool
                        -- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
                        -- TODO: review!
                        -- The first action makes a heterogenous list of [unpackNum, unpackStr, unpackBool], and then maps the 
                        -- partially applied (unpackEquals arg1 arg2) over it. This gives a list of booleans, so we use the 
                        -- Prelude function or to return true if any single one of them is true.
                        -- Question: isn't it a ThrowsError Of Bool? 
                        primativeEquals <- liftM or $ mapM (unpackEquals arg1 arg2) unpackFunctions
                        eqvEquals <- eqv [arg1, arg2]
                        return $ Bool $ (primativeEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
                                                        
                        
-- store all unpack functions in a list and mapM around them
-- by default Haskell only supports Heterogeneous list so use Existential Types.

unwordsList :: [LispVal] -> String

-- see 
-- :t Prelude.unwords  [String] -> String,
-- :t map (a -> b) -> [a] -> b
--  point-free style: writing definitions purely in terms of function composition and partial application
unwordsList = unwords . map showVal

-- takes a primitive Haskell function (often an operator section) and wraps it 
-- with code to unpack an argument list, apply the function to it, and wrap the result up in our Number constructor.
--  Existential Types – that lets us create a heterogenous list, subject to typeclass constraints.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n

unpackNum (String str) = let parsed = reads str in
                                        if null parsed
                                            then throwError $ TypeMismatch "number" $ String str
                                            else return $ fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- unpackers
-- lispValToHaskell :: LispVal -> a
-- lispValToHaskell (Number n) = read n
-- lispValToHaskell (String s) = s
-- lispValToHaskell (Bool b) = b

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val@(_) = throwError $ TypeMismatch "bool" val

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (Number n) = return . show $ n
unpackString (Number b) = return . show $ b
unpackString val@(_) = throwError $ TypeMismatch "string" val

-- Adding basic primitives
primatives :: [(String, [LispVal] -> ThrowsError LispVal)]
primatives = numbericPrimatives ++ typeCheckPrimatives ++ booleanPrimatives ++ listPrimatives ++ equalPrimatives

listPrimatives :: [(String, [LispVal] -> ThrowsError LispVal)] 
listPrimatives = [("car", car),
                  ("cdr", cdr),
                  ("cons", cons)
                 ]
                  
equalPrimatives :: [(String, [LispVal] -> ThrowsError LispVal)] 
equalPrimatives = [("eq?", eqv),
                  ("eqv?", eqv),
                  ("equal?", equal)
                 ]
                  
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

booleanPrimatives :: [(String, [LispVal] -> ThrowsError LispVal)]                       
booleanPrimatives = [
                     ( "=", numberBoolBinop  (==) ),
                     ( "/=", numberBoolBinop (/=) ),
                     ( ">", numberBoolBinop  (>)  ),
                     ( "<", numberBoolBinop  (<)  ),
                     ( ">=", numberBoolBinop (>=) ),
                     ( "<=", numberBoolBinop (<=) ),
                     ( "&&", booleanBinop    (&&) ),
                     ( "||", booleanBinop    (||) ),                     
                     ( "string=?", stringBoolBinop  (==) ),
                     ( "string<?", stringBoolBinop  (<)  ),
                     ( "string>?", stringBoolBinop  (>)  ),
                     ( "string<=?", stringBoolBinop (<=) ),
                     ( "string>=?", stringBoolBinop (>=) )
                     ]

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp fn [v] = fn v

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do -- Because each argument may throw a type mismatch, we have to unpack them sequentially,
                                      left  <- unpacker $ args !! 0
                                      right <- unpacker $ args !! 1
                                      -- "Any function can be turned into an infix operator by wrapping it in backticks (`op`)." -- op left right
                                      return $ Bool $ left `op` right
    
-- operations definitions
-- numberBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal    
-- numberBoolBinop op [] = throwError $ NumArgs 2 [] -- error case 
-- numberBoolBinop op args = Bool $ foldl1 op args

numberBoolBinop = boolBinop unpackNum
booleanBinop = boolBinop unpackBool
stringBoolBinop = boolBinop unpackString

-- booleanBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
-- booleanBinop op [] = throwError $ NumArgs 2 []
-- 
-- stringBoolBinop :: (String -> String -> String) -> [LispVal] -> ThrowsError LispVal
-- stringBoolBinop op [] = throwError $ NumArgs 2 []         

-- type predicates
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
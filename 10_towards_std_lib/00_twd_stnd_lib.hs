{-# LANGUAGE ExistentialQuantification #-}
module Main where
import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct, readInt)
import Data.Char (digitToInt)
import Control.Monad (liftM)
import Control.Monad.Error -- is deprecated: Use Control.Monad.Except instead

import Data.IORef
import System.IO

{--
    
    Now with IO!
    
    > compile:  ghc -o srcfiles/repl_stnlib /10_towards_std_lib/00_twd_stnd_lib.hs
    > run:      srcfiles/repl_fun
    >           Lisp>>> (define (f x y) (+ x y))
                (lambda ("x" "y"..)
                Lisp>>> (f 1 2)
                3
                Lisp>>> (f 1 2 3)
                [ Expected : 2, found [1,2,3]]
                Lisp>>> (f 1)
                [ Expected : 2, found [1]]
                Lisp>>> (define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
                (lambda ("x"..)
                Lisp>>> (factorial 10)
                3628800
                Lisp>>> (define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
                (lambda ("inc"..)
                Lisp>>> (define my-count (counter 5))
                (lambda ("x"..)
                Lisp>>> (my-count 3)
                8
                Lisp>>> (my-count 6)
                14
                Lisp>>> (my-count 5)
                19
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
               | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
               | Func { params :: [String], vararg :: (Maybe String),
                        body :: [LispVal], closure :: Env }
               | IOFunc ([LispVal] -> IOThrowsError LispVal)
               | Port Handle
               
showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom   s) = s
showVal (Number x) = show x
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "\'" ++ [c] ++ "\'"
showVal (Float f) = show f
showVal (List xs) = "(" ++ show xs ++ ")"
showVal (DottedList xs x) = "(" ++ show xs ++ " . " ++ show x ++ ")"
showVal (PrimitiveFunc _) = "<Primative>"
showVal (IOFunc args) = "<IO Primative>"
showVal (Port args) = "<IO Port>"

-- This is an example of pattern-matching for records
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing  -> ""
            Just arg -> " . " ++ arg
        ) ++ "..)"


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
    noMsg  = Default "An error has occurred"
    strMsg = Default

-- represents a Type that may throw an Error
-- Type constructors are curried just like functions, and can also be partially applied.
type ThrowsError = Either LispError

-- convert all errors to a String representation
-- "catchError, which takes an Either action and a function that turns an error into another Either action. 
--  If the action represents an error, it applies the function, which you can use to, e.g. turn the 
--  error value into a normal one via return or re-throw as a different error.""
-- >:t catchError Control.Monad.Error.Class.MonadError e m => m a -> (e -> m a) -> m 
trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- helper function for making Func type
makeFunc varargs env params body = return $ Func ( map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgFunc = makeFunc . Just . showVal

-- Evaluating numbers, strings, booleans, and quoted lists is fairly simple: return the datum itself.
-- thread the env through 
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)  = return val
eval env val@(Number _)  = return val
eval env val@(Bool _)    = return val
eval env val@(Float _)   = return val
-- Quoted List
-- TODO: confused on this type
eval env (Atom id) = getVar env id
eval env (List ([Atom "quote", val])) = return val
-- four elem list pattern match
eval env (List ([Atom "if", pred, conseq, alt])) = do
                                                    result <- eval env pred
                                                    case result of
                                                       Bool False -> eval env alt
                                                       otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var

-- eval for functions
-- bound functions
eval env (List (Atom "define" : List (Atom var : params) : body )) = 
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgFunc varargs env params body >>= defineVar env var
-- anonomous function
eval env (List (Atom "lambda" : List params : body)) = 
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = 
    makeVarArgFunc varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgFunc varargs env [] body

eval env (List [Atom "load", String filename]) = 
     load filename >>= liftM last . mapM (eval env) -- mapping passing along the environment, take that last one as that is the most recently updated env
    
-- We have to recursively evaluate each argument, so we map eval over the args. 
-- This is what lets us write compound expressions like (+ 2 (- 3 1) (* 5 4)). 
-- Then we take the resulting list of evaluated arguments, and pass it and the original function to apply:
--eval (List (Atom fn : args)) = apply fn $ map eval args
-- Now that wrapped in Monadic ThrowsError
--eval env (List (Atom fn : args)) = mapM eval env args >>= apply fn
--eval env (List (Atom fn : args)) = mapM (eval env) args >>= liftThrows . apply fn
eval env (List (function : args)) = do
                                     func <- eval env function
                                     argVals <- mapM (eval env) args
                                     apply func argVals

-- catch all eval
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
-- apply now takes a LispVal to make it easier to pass around primative function
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
-- apply fn args = maybe (Bool False) ($ args) $ lookup fn primatives             
apply (PrimitiveFunc fn) args = liftThrows $ fn args
apply (IOFunc fn) args = fn args

-- function's closure (not the current environment – this is what gives us lexical scoping
apply (Func params varargs body closure) args =
    if length params /= length args && varargs == Nothing
        then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length 
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of 
                                    Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                    Nothing -> return env

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode 

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True) -- this about this, is this iterating through the list?
cloasePort _          = return $ Bool False

-- Notice how hGetLine port is of type IO String yet readExpr is of type String -> ThrowsError LispVal, 
-- so they both need to be converted (with liftIO and liftThrows, respectively) to the IOThrowsError monad. 
-- Only then can they be piped together with the monadic bind operator.
readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin] -- TODO getting lost
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr 
 
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]     = writeProc [obj, Port stdout]
-- We don't have to explicitly call show on the object we're printing, because hPrint takes a value of type Show a. 
-- It's calling show for us automatically. implicit ;)
writeProc [obj, Port port] = (liftIO $ hPrint port obj) >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


    
-- maybe (throwError $ NotFunction "Unrecognized primitive function args" fn) 
--                                 ($ args) 
--                                 (lookup fn primatives)             
    
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

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val@(_) = throwError $ TypeMismatch "bool" val

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (Number n) = return . show $ n
unpackString (Number b) = return . show $ b
unpackString val@(_) = throwError $ TypeMismatch "string" val

-- Adding basic primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = numbericPrimitives ++ typeCheckPrimitives ++ booleanPrimitives ++ listPrimitives ++ equalPrimitives

-- Adding IO Primitives
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

listPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)] 
listPrimitives = [("car", car),
                  ("cdr", cdr),
                  ("cons", cons)
                 ]
                  
equalPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)] 
equalPrimitives = [("eq?", eqv),
                   ("eqv?", eqv),
                   ("equal?", equal)
                  ]
                  
numbericPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numbericPrimitives = [("+", numericBinOp (+)),
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


typeCheckPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
typeCheckPrimitives = [
                        ("symbol?", unaryOp symbolp),
                        ("string?", unaryOp stringp),
                        ("number?", unaryOp numberp),
                        ("boolean?", unaryOp booleanp)
                       ]

booleanPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]                       
booleanPrimitives = [
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

numberBoolBinop = boolBinop unpackNum
booleanBinop = boolBinop unpackBool
stringBoolBinop = boolBinop unpackString     

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

{-- 
    Adding variables need to hold them in an environment that can be passed around.
    state threads in Haskell State Monad and IORef s
    We need IORefs for both the list itself and for individual values because there are two ways that the program can mutate the environment:
    1. `set!` to change the value of an individual variable,  a change visible to any function that shares that environment
    2. Or it might use `define` to add a new variable, which should be visible on all subsequent statements.
--}
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

-- handle Errors that can happen in IO, wraps two monads: IORef and ErrorT
-- Type constructor like ThrowsError, no type bound to IO.
type IOThrowsError = ErrorT LispError IO

{--
  . Haskell already provides a mechanism, lifting, to bring values of the lower type (IO) 
    into the combined monad. Unfortunately, there's no similar support to bring a value of the 
    untransformed upper type into the combined monad, so we need to write it ourselves:
--}
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err -- ??
liftThrows (Right val) = return val 

-- helper function to run top-level IOThrowsError action, returning an IO action.
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

{-- 
    function to determine if a given variable is already bound in the environment, necessary for proper handling of define
    Data.IORef.readIORef :: GHC.IORef.IORef a -> IO a
    "we need to use the const function because maybe expects a function to perform on the result and not just a value"
--}
isBound :: Env -> String -> IO Bool 
isBound envRef varname = readIORef envRef >>= return . maybe (False) (const True) . lookup varname

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef varname = do
                      env <- liftIO $ readIORef envRef    
                      maybe (throwError $ UnboundVar "Getting an unbound variable" varname)
                            (liftIO . readIORef) -- <-- [ IORef a -> IO a ]  IORef is a mutable variabl in the IO Monad
                            (lookup varname env)
                               
-- getVar envRef var = if (isBound envRef var)
--                       then readIORef envRef
--                       otherwise

{--
    we want to change the variable instead of just reading it. The writeIORef action provides a means for this, 
    but takes its arguments in the wrong order (ref -> value instead of value -> ref). So we use the built-in 
    function flip to switch the arguments of writeIORef around, and then pass it the value. Finally, 
    we return the value we just set, for convenience
--}
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
-- if it's bound reset it, else
setVar envRef varname value = do 
                            env <- liftIO $ readIORef envRef -- note forget to liftIO
                            maybe (throwError $ UnboundVar "Getting an unbound variable" varname)
                                  (liftIO . (flip writeIORef value)) -- ???
                                  (lookup varname env)
                            return value
                                  
{--
    define - sets a variable if already bound or creates a new one if not.
    We create an IO action (via do-notation) that creates a new IORef to hold the new variable, 
    reads the current value of the environment, then writes a new list back to that variable consisting 
    of the new (key, variable) pair added to the front of the list. Then we lift that whole do-block into 
    the IOThrowsError monad with liftIO.
--}
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef varname value = do
                                isBound <- liftIO $ isBound envRef varname
                                if (isBound)
                                    then setVar envRef varname value >> return value
                                    else liftIO $ do
                                                    valueRef <- newIORef value
                                                    env <- readIORef envRef 
                                                    writeIORef envRef ((varname, valueRef) : env)
                                                    return value

{--
    bind a whole bunch of variables at once, 
    Env :: IORef [(String, IORef LispVal)]
--} 
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef -- bindinds returns IO then >>= picks out internal and creates IO IORef env
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)   -- not indent, if this is off then parsing error on '='
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives  -- TODO: This is like casting
                                                     ++ map (makeFunc IOFunc) ioPrimitives) -- think aobut map here!
            where makeFunc constructor (var, fn) = (var, constructor fn)    

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                                Left err -> throwError $ Parser err
                                Right val -> return val 
-- ? Let's change it so that it wraps and throws the original ParseError:
readExpr :: String -> ThrowsError LispVal
--readExpr input = case parse (parserExpr) "(Lisp)" input of
--                    Left err  -> throwError $ Parser err
--                    Right val -> return val

--  We'll be using readExpr in our REPL to read single expressions
readExpr = readOrThrow parserExpr -- TODO: where is parseExpr coming from?
-- we'll be using readExprList from within load to read programs
readExprList = readOrThrow (endBy parserExpr spaces)
    
-- a function that prints out a string and immediately flushes the stream; 
-- otherwise, output might sit in output buffers and the user will never see prompts or results.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Then, we create a function that prints out a prompt and reads in a line of input:
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Pull the code to parse and evaluate a string and trap the errors out of main into its own function:
-- Control.Monad.liftM :: Monad m => (a1 -> r) -> m a1 -> m r 
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
--evalString env expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

                
-- And write a function that evaluates a string and prints the result:
evalAndPrint :: Env -> String -> IO ()  
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ signalStop prompt action = do
                                result <- prompt
                                if signalStop result
                                    then  return ()
                                else action result >> until_ signalStop prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint 

--runOne :: String -> IO()
--runOne input = primitiveBindings >>= flip evalAndPrint input

runOne :: [String] -> IO ()
runOne args = do -- IO Env      bindVars :: Env -> [(String, LispVal)] -> IO Env
        env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] -- TODO review add List infront of [ ] and understand type error
        (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)] ))
        >>= hPutStrLn stderr
        
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
-- main = do
--      args <- getArgs
--      case length args of
--          0 -> runRepl
--          1 -> runOne $ args !! 0
--          otherwise -> putStr "Enter either 0 or 1 args "

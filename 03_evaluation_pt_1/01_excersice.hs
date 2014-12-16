module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad(liftM)
{--

    compile: ghc -o srcfiles/eval  03_evaluation_pt_1/01_excersice.hs
    run:     srcfiles/eval "\"coffee\""
            srcfiles/eval "(+ 2 4)"
            srcfiles/eval "(symbol? +)"
            srcfiles/eval "(string? \"sdf\")"
            srcfiles/eval "(boolean? #t)"

--}
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal = String String |
               Character Char |
               Number Integer |
               Float Float |
               Atom String |
               Bool Bool |
               List [LispVal] |
               DottedList [LispVal] LispVal
               
showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Atom s) = s
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (Character c) = [c]
showVal (List xs) = "List: [" ++ show xs ++ "]"
showVal (DottedList xs x) = "DottedList: [" ++ show xs ++ show x ++ "]"

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Atom _) = val
eval val@(Character _) = val
eval val@(Bool _) = val

eval (List([Atom "quote", val])) = val
-- We have to recursively evaluate each argument, so we map eval over the args. 
-- This is what lets us write compound expressions like (+ 2 (- 3 1) (* 5 4)). 
-- Then we take the resulting list of evaluated arguments, and pass it and the original function to apply:
eval (List (Atom fn : args)) = apply fn $ map eval args

apply :: String -> [LispVal] -> LispVal
apply fn args = maybe (Bool False) ($ args) $ lookup fn primatives

primatives :: [(String, [LispVal] -> LispVal)]
primatives = numericPrimatives ++ typeCheckPrimatives

numericPrimatives = [("+", numbericBinOp (+)),
                     ("-", numbericBinOp (-)),
                     ("*", numbericBinOp (*)),    
                     ("/", numbericBinOp div),
                     ("mod", numbericBinOp mod),
                     ("quotient", numbericBinOp quot),
                     ("remainder", numbericBinOp rem)
                     ]
-- Add primitives to perform the various type-testing functions of R5RS: symbol?, string?, number?, etc.             
typeCheckPrimatives :: [(String, [LispVal] -> LispVal)]
typeCheckPrimatives = [
                        ("symbol?", checkType symbolp),
                        ("string?", checkType stringp),
                        ("number?", checkType numberp),
                        ("boolean?", checkType booleanp)
                       ]

symbolp, stringp, numberp, booleanp :: LispVal -> LispVal

symbolp(Atom _) = Bool True
symbolp(_) = Bool False
stringp(String _) = Bool True
stringp(_) = Bool False
numberp(Number _) = Bool True
numberp(_) = Bool False
booleanp(Bool _) = Bool True
booleanp(_) = Bool False

checkType :: (LispVal -> LispVal) -> [LispVal] -> LispVal
checkType fn [v] = fn v

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = read s :: [(Integer, String)] in
                            if null parsed
                                then 0
                                else fst $ parsed !! 0
                                
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numbericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numbericBinOp op params = Number $ foldl1 op $ map unpackNum params


-- parsers

parserString :: Parser LispVal
parserString = do
                char '"'
                spaces
                s <- many(digit <|> letter <|> space)
                char '"'
                return $ String s

parserAtom :: Parser LispVal
parserAtom = do
              first <- (letter <|> symbol)
              rest <- many(letter <|> symbol <|> digit)
              let atom = first : rest
              return $ Atom atom

parserBool :: Parser LispVal
parserBool = try $ do
                    char '#'
                    b <- char 't' <|> char 'f'
                    return $ Bool $ case b of
                                        't' -> True
                                        'f' -> False

parserDecimal :: Parser LispVal
parserDecimal = do
                digits <- many1 digit
                return $ Number . read $ digits

parserFloat :: Parser LispVal
parserFloat = try $ do
                      maybeDigits <- many digit
                      char '.'
                      decimal <- many1 digit
                      return $ Float . read $ case maybeDigits of
                                                [] -> "0." ++ decimal
                                                _ ->  maybeDigits ++ ['.'] ++ decimal

parserNumber :: Parser LispVal
parserNumber = parserDecimal

parserList :: Parser LispVal
parserList = liftM List $ sepBy parserExprs spaces
-- parserList = do
--               xs <- many(parseExprs)
--               return $ List xs
              
parserDottedList :: Parser LispVal
parserDottedList = do
                    car <- endBy parserExprs spaces
                    cdr <- char '.' >> spaces >> parserExprs
                    return $ DottedList car cdr
                    
parserExprs :: Parser LispVal
parserExprs = parserString 
            <|> parserFloat
            <|> parserBool
            <|> parserAtom
            <|> parserNumber
            <|> do
                 char '('
                 ls <- try parserList <|> parserDottedList
                 char ')'
                 return ls

readExpr :: String -> LispVal
readExpr input = case parse parserExprs "(Lisp)" input of
                    Left err -> String $ "No Match" ++ show err
                    Right val -> val 

main :: IO()
-- main = getArgs >>= \args -> print . eval $ args !! 0
-- Note if you don't pass the value through it seems to be implicitly passed i.e. head can be used
main = getArgs >>= print . eval . readExpr . head
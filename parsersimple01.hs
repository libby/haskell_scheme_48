import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- ghc -o srcfiles/parser01  parsersimple01.hs
-- ghc --make -o simple_parser parsersimple01.hs
-- srcfiles/parser01  "sdf () sdfjkloe lll \r dsdl"
--This is another example of a monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" 

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                | Character Char

-- print helpers
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number num) = "\"" ++ show  num ++ "\"" 
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList  xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- A full treatment of typeclasses is beyond the scope of this tutorial; you can find more information in    
instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList =  unwords . map showVal

--  Characters are objects that represent printed characters such as letters and digits. 
-- Characters are written using the notation #\<character> or #\<character name>. For example:               
parseCharacterLiteral :: Parser LispVal
parseCharacterLiteral = do
                         char '#'
                         char '\\' 
                         x <- many ( noneOf " ")
                         return $ Character $ case x of
                                                "space" -> ' '
                                                "newline" -> 'a'
                                                "a" -> 'a'
                        

-- We're back to using the do-notation instead of the >> operator.
-- In general, use >> if the actions don't return a value, 
-- >>= if you'll be immediately passing that value into the next action, 
-- and do-notation otherwise.
-- each line of a do-block must have the same type, but the result of our String constructor is just a plain old LispVal
-- "adsf\"" "adsf"
escapedChar :: Parser Char  -- monad Char
escapedChar = do
                char '\\'    -- a backslash
                x <- oneOf "\\\""
                return x -- escaped char

-- Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters
specialChar :: Parser Char 
specialChar = do
            char '\\'
            x <- oneOf "nrt\\"
            return $ case x of -- escaped char
                        '\\' -> x
                        '"' -> x
                        'n' -> '\n'
                        'r' -> '\r'
                        't' -> '\t'
                    

-- Parser monad of LispVal
-- noneOf "\"" with a new parser action that accepts either a non-quote character or a backslash followed by a 
parseString :: Parser LispVal
-- original
parseString = do
                char '"'
                x <- many (specialChar <|> escapedChar <|> noneOf "\"\\" )
                char '"'
                return $ String x
                
-- parseString = do 
--                 char '"'
--                 --x <- many (noneOf "\"") -- many (noneOf "") >>= \x -> ...
--                 x <- many (satisfy \s -> s == '\\s')
--                 char '"'
--                 -- lift into the Parser monad, $ operator is infix function application: it's the same as if we'd written return (String x) :/
--                 return $ String x
--                 
--                 --f(x) = h(g(x))
--                 --f = h . g

-- An atom is a letter or symbol, followed by any number of letters, digits, or symbols:
parseAtom :: Parser LispVal
-- , the choice operator <|>. This tries the first parser, then if it fails, tries the second.
parseAtom = do
                first <- letter <|> symbol
                rest <- many ( letter <|> symbol <|> digit )
                let atom = first:rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _ -> Atom atom

parseNum :: Parser LispVal
-- it's easiest to read this backwards, since both function application ($) and function composition (.) associate to the right.
-- We need a way to tell it to just operate on the value inside the monad, giving us back a Parser LispVal. 


-- parNum with liftM  - original
-- The standard function liftM does exactly that, so we apply liftM to our Number
-- parseNum = liftM (Number . read) $ many1 digit

-- Exercises
-- do verison
parseNum = do
           xs <- many1 digit
           return $ (Number . read) xs

-- parseNum = do
--            numString <- many1 digit
--            let num = (read numString) :: Integer
--            return $ Number num

-- TODO: check, is this wierd?
--parseNum = (many1 digit) >>= \d -> return $ Number (read d :: Integer)

-- parseNumber = many1 digit >>= \x -> (return . Number . read) x
-- parseNumber = many1 digit >>= return . Number . read
-- like scala if it takes one arg it seems that haskell can infer that and will automaticallt pass it along
    
-- algebraic data type: it defines a set of possible values that a variable of type LispVal can hold    
-- Constructors and types have different namespaces, so you can have both a constructor named String and a type named String

-- parser that accepts either a string, a number, or an atom:
parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseNum <|> parseString

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces    

-- DottedList [LispVal] LispVal () . ()
parseDottedList :: Parser LispVal                
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail

parseSingleQuote :: Parser LispVal
parseSingleQuote = do
                    char '\''
                    x <- parseExpr
                    return $ List [Atom "quote", x]

-- pass symbol to parse function parse(symbols, nameforLogging, input)
-- >> ("bind") operator where we mentioned that it was used behind the scenes 
-- to combine the lines of a do-block. Here, we use it explicitly to combine our whitespace and symbol parsers.
-- However, bind has completely different semantics in the Parser and IO monads. In the Parser monad, bind means 
-- "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails."
readExpr :: String -> String
-- case parse (spaces >> symbol) "lisp" input of
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right valM -> "found value: " ++ show valM

--    do v <- valM putStrLn $ (String . read) v
          
    
main :: IO()
-- Haskell's do expressions provide a convenient syntax for writing monadic expressions
main = do 
        args <- getArgs
        putStrLn (readExpr (args !! 0))
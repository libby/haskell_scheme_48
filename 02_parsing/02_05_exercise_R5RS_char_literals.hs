module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

{--

    http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
    5. Add a Character constructor to LispVal, and create a parser for character literals as described in R5RS.
    http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4
    
    #\a	; lower case letter
    #\A	; upper case letter
    #\(	; left parenthesis
    #\ 	; the space character
    #\space	; the preferred way to write a space
    #\newline	; the newline character
    
    Case is significant in #\<character>, but not in #\<character name>.    
    
    compile: ghc -o srcfiles/02_05_exercise_R5RS_char_literals 02_parsing/02_05_exercise_R5RS_char_literals.hs
    run:     srcfiles/02_05_exercise_R5RS_char_literals "#\a" 
    
--}

newtype LispVal = Character Char 
  deriving Show 

-- #\a 
lowerCase :: Parser Char
lowerCase = do
              c <- try(string "#\\" >> oneOf ['a'..'z'])
              return c

-- #\A	 
upperCase :: Parser Char
upperCase = do
              c <- try(string "#\\" >> oneOf ['A'..'Z'])
              return c      
        
-- #\(
leftParen :: Parser Char
leftParen = do
              c <- try(string "#\\" >> char '(')
              return c
              
-- #\ note should go last in the chain
spaceChar :: Parser Char
spaceChar = do
              c <- string "#\\"
              return ' '

-- #\space the preferred way to write a space
spaceWord :: Parser Char
spaceWord = do
             try(string "#\\space")
             return ' '

-- #\newline
newLine :: Parser Char
newLine = do
            try(string "#\\newline")
            return '\r'
            
    
parseCharLiteral :: Parser LispVal
parseCharLiteral = do
                    c <- spaceWord <|> newLine <|> lowerCase <|> 
                         upperCase <|> leftParen <|> spaceChar
                    return $ Character c

readExpr :: String -> String
readExpr input = case parse parseCharLiteral "(Lisp)" input of
                    Left err -> "Not Found " ++ show err
                    Right val -> "Found Val" ++ show val

main :: IO()
main = do
        args <- getArgs
        putStrLn(readExpr (args !! 0))


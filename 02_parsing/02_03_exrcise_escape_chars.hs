module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

{--

 escape all the special chars

 compile: ghc -o srcfiles/02_03_exrcise_escape_chars 02_parsing/02_03_exrcise_escape_chars.hs
 run:     srcfiles/02_03_exrcise_escape_chars "\"coffee \\" internal text \\" \""
 
--}

newtype LispVal = String String
 deriving Show

parserEscapedChars :: Parser Char
parserEscapedChars = do
                       char '\\'              -- potential escaped char
                       c <- oneOf "ntr\\\""   -- is the dude special
                       return $ case c of
                                  'r' -> '\r'
                                  't' -> '\t'
                                  'n' -> '\n'
                                   _ -> c

parserWithEscapedChars :: Parser LispVal
parserWithEscapedChars = do
                          char '"'
                          x <- many(parserEscapedChars <|> noneOf("\"\\"))
                          char '"'
                          return $ String(x)
                          

readExprs :: String -> String
readExprs input = case parse (parserWithEscapedChars) "(Lisp)" input of
                    Left err  -> "Not Found "   ++ show err
                    Right val -> "Found Value " ++ show val

main :: IO()
main = do
        args <- getArgs
        putStrLn(readExprs(args !! 0))
module Main where

import System.Environment
-- import qualified to make it obvious when using Parsec
import qualified Text.Parsec as Parsec
-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))
-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

{--
    based on: http://unbui.lt/#!/post/haskell-parsec-basics
--}

someText = "Hello Hello Hello World World World"
-- utility function
parse rule text = Parsec.parse rule  "(source)" text

{--
 > ghci
 > import qualified Text.Parsec as Parsec
 > let someText = "Hello Hello Hello World World World"
 > parse rule text = Parsec.parse rule  "(source)" text
 > parse (Parsec.char 'e') someText
 
--}

{--
    compile: ghc -o srcfiles/000_playing_around_with_parsec 02_parsing/haskell_parsec_basics/000_playing_around_with_parsec.hs
    run:    srcfiles/000_playing_around_with_parsec
--}
main :: IO()
main = do
        let result = parse (Parsec.char 'H') "Hello"
        case result of
            Left err -> putStrLn "Error"
            Right match -> putStrLn "Match"
        let errorResult = parse (Parsec.char 'e') "Hello"
        case errorResult of
            Left err -> putStrLn "no 'e' in Hello"
            Right match -> putStrLn "we have a match!"
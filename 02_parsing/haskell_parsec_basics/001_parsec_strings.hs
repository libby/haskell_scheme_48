module Main where
import System.Environment
import qualified Text.Parsec as Parsec

parse rule text = Parsec.parse rule "(fileGoesHere)" text

{--
    Parse.string - This function returns a rule that attempts to match the provided string of characters:
    > parse (Parsec.string "Hel") someText
    
    
    Parse.string
    "The parser will consume characters from the input one by one until all characters match or one of them is not as expected. 
    ... This consuming of characters will become significant when multiple rules are chained together."
    
    compile: ghc -o srcfiles/001_parsec_strings 02_parsing/haskell_parsec_basics/001_parsec_strings.hs
    run:    srcfiles/001_parsec_strings
--}

main :: IO()
main = do
        let matchResult = parse (Parsec.string "Hel") "Hello Pixel"
        case matchResult of                         -- TODO: abstract this out into a function?
            Left err -> putStrLn ">.< Error! Error!"
            Right match -> putStrLn "=) Match! Match! 'Hel' is in 'Hello Pixel'"

        let errorResult = parse (Parsec.string "elo") "Hello Pixel"
        case errorResult of
            Left err -> putStrLn ">.< Error! Error! 'elo' is not in 'Hello Pixel' "
            Right match -> putStrLn "=) Match! Match!"
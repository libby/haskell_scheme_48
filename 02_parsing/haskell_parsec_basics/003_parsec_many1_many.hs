module Main where
import System.Environment
import qualified Text.Parsec as Parsec

-- import Text.Parsec (<|>) -- hmmm cannot do this...breaks Parsec.<|>

{--
    Parsec.many and Parsec.many1: 
        Parsec.many tries to run whatever rule it is given as an argument over and over until it fails.
        Even if the rule matches no times, many will return without error, 
        but just give back an empty result.
    
    ghci> parse (Parsec.many (Parsec.char 'h')) "hhhheeelllooo!"
        Right 'hhhh'
    ghci> parse (Parsec.many (Parsec.char 'e')) "hhhheeelllooo!"
        Right ''
    ghci> parse (Parsec.many Parsec.letter) "hhhheeelllooo!"
        Right 'hhhheeelllooo'
    Always returns a Right, so happy happy even with no match.
    
    Parsec.many1 is similar except that the rule it's provided must match at least once for it to return successfully:
    ghci> parse (Parsec.many1 Parsec.letter) "hello!!"
        Right 'hello'
    ghci> parse (Parsec.many1 Parsec.letter) "75 hello's!"
        Left "(source)" (line 1, column 1):
        unexpected "7"
        expecting letter

    Parsec.count
    ghci> parse (Parsec.count 4 Parsec.letter) "ahoythere"
        Right "ahoy"
    ghci> parse (Parsec.count 4 Parsec.letter) "aho"
        Left "(source)" (line 1, column 4):
        unexpected end of input
        expecting letter    
    
    Parsec.manyTill    
    ghci> parse (Parsec.manyTill Parsec.letter Parsec.digit) "hello12345"
        Right "hello"
    ghci> parse (Parsec.manyTill Parsec.letter Parsec.digit) "12345"
        Right ""
    ghci> parse (Parsec.manyTill Parsec.letter Parsec.digit) "hello 12345"
        Left "(source)" (line 1, column 6):
        unexpected " "
        expecting digit or letter

    "When we start stringing together rules in sequence, 
    it becomes increasingly important what we consume and what we leave ready for the next rule to have a go at."
    
    > ghc -o srcfiles/003_parsec_many1_many 02_parsing/haskell_parsec_basics/003_parsec_many1_many.hs
    > srcfiles/003_parsec_many1_many
    
--}
parse rule text = Parsec.parse rule "(file being parsed)" text

stringToMatch = "Hello\"quote\" There "

main :: IO()
main = do
        -- let matchResult = parse (Parsec.many $ Parsec.char 'c' Parsec.<|> Parsec.char 'H') stringToMatch
        --        case matchResult of
        --            Left err -> putStrLn ">.< boo for ( many Parsec.char 'c' <|> Parsec.char 'H' ) matching 'Hello There' "
        --            Right match -> putStrLn $ "=) found [" ++ match ++ "] ( many Parsec.char 'c' <|> Parsec.char 'H' ) matched 'Hello There' "  
        --        
        --        let match2Result = parse (Parsec.many $ Parsec.char 'c' Parsec.<|> Parsec.char 'H' Parsec.<|> Parsec.char 'e') stringToMatch
        --        case match2Result of
        --            Left err -> putStrLn ">.< boo for ( many Parsec.char 'c' <|> Parsec.char 'H' ) matching 'Hello There' "
        --            Right match -> putStrLn $ "=) found [" ++ match ++ "] ( many Parsec.char 'c' <|> Parsec.char 'H' ) matched 'Hello There' "
        
        let parseRuleWithQuotes = (Parsec.many $ Parsec.char 'H' Parsec.<|> 
                                                Parsec.char 'e' Parsec.<|> Parsec.char 'l' Parsec.<|> 
                                                Parsec.char 'l' Parsec.<|> Parsec.char 'o' Parsec.<|>
                                                Parsec.char '\"' Parsec.<|> Parsec.noneOf "\"") -- This will match everything b/c it'll keep retrying noneOf on no match 
        let matchUpToQuoteResult = parse parseRuleWithQuotes stringToMatch
        case matchUpToQuoteResult of
          Left err -> putStrLn ">.< boo for ( many Parsec.char 'c' <|> Parsec.char 'H' ) matching 'Hello There' "
          Right match -> putStrLn $ "=) found [" ++ match ++ "] ( many Parsec.char 'c' <|> Parsec.char 'H' ) matched 'Hello There' "
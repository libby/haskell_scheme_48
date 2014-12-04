module Main where

import System.Environment
import qualified Text.Parsec as Parsec

parse rule text = Parsec.parse rule "(parsing file name)" text

{--
    Parsec.oneOf - for matching a range of Chars
    
    Parsec.noneOf - dual of oneOf 
    
    compile: ghc -o srcfiles/002_parsec_oneOf_noneOf 02_parsing/haskell_parsec_basics/002_parsec_oneOf_noneOf.hs
    run:    srcfiles/002_parsec_oneOf_noneOf
--}
-- Parsec.oneOf ['a'..'z']
main :: IO()
main = do
        let matchResult = parse (Parsec.oneOf "eHxo") "Hello There"
        case matchResult of
            Left err -> putStrLn ">.< nooo matches none of 'eHxo' where at start of stream 'Hello There' "
            Right match -> putStrLn " =) there was oneOf 'eHxo' at the start of stream  'Hello There' "
        
        let matchRangeResult = parse (Parsec.oneOf ['A'..'Z']) "Hello There"    
        case matchRangeResult of
            Left err -> putStrLn ">.< nooo matches for Range ['A'..'Z'] where at start of stream 'Hello There' "
            Right match -> putStrLn " =) there was oneOf Range ['A'..'Z'] at the start of stream  'Hello There' "    
        
        let failedRangeResult = parse (Parsec.oneOf ['a'..'z']) "Hello There"    
        case failedRangeResult of
            Left err -> putStrLn ">.< nooo matches for Range ['a'..'z'] where at start of stream 'Hello There' "
            Right match -> putStrLn " =) there was oneOf Range ['a'..'z'] at the start of stream  'Hello There' "
        
        let matchAnyChar = parse (Parsec.anyChar) "=blahblah"
        case matchAnyChar of
            Right match -> putStrLn "=) Well of course anyChar matches '=blahblah' "

        let failedResult = parse (Parsec.oneOf "xz*nl") "Hello There"
        case failedResult of
            Left err -> putStrLn ">.< nooo matches none of 'xz*nl' where at start of stream 'Hello There' "
            Right match -> putStrLn "=) there was oneOf 'eHxo' at the start of stream  'Hello There' "
        
        -- noneOf Matches
        let matchNoneOfResult = parse (Parsec.noneOf "qwertyuiopasdfg") "Hello There"
        case matchNoneOfResult of
            Right match -> putStrLn "=) noneOf 'qwertyuiopasdfg' where at the beginning of 'Hello There'"

        let failedNoneOfResult = parse (Parsec.noneOf "qwertyHu") "Hello There"
        case failedNoneOfResult of
            Left err -> putStrLn ">.< nooo matches for noneOf 'qwertyHu' where at start of stream 'Hello There'"
        
        
        
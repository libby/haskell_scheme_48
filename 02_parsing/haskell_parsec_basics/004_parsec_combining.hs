--module Parsec.Libby.Combining where

import System.Environment 
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>)) -- like or except if one parse succeed on the first match, it is selected, if it later fails, the whole chain fails.
-- Control.Applicative <|>   
 
parse rule text = Parsec.parse rule "(file being parsed)" text

-- Do not understand there seem to be three Typeparams being passed in: https://github.com/aslatter/parsec/blob/master/Text/Parsec/Prim.hs
-- type Parsec s u = ParsecT s u Identity  // seem to only have s and u free
--  ...but then again takes a Parsec String () and returns a monad (String,String) >.<
-- turns out Identity is a Mondic Type Class so the internal value is left "free" and that is what (String, String) appears to be.

{--
-- I have to import the identity monad to use in the ParsecT definition:
import Control.Monad.Identity (Identity)

-- This looks for letters, then spaces, then digits.
-- we then return letters and digits in a tuple. 
myParser :: Parsec.Parsec String () (String,String)
let myParser = (do
                letters <- Parsec.many1 Parsec.letter
                Parsec.spaces
                digits <- Parsec.many1 Parsec.digit
                return (letters,digits) ) 


myParser1 :: Parsec.ParsecT String () Identity (String,String)
myParser1 = myParser

myParser2 :: Parsec.Parsec String () (String,String)
myParser2 = myParser
> let myParser2 = myParser

> ghc -o srcfiles/004_parsec_combining 02_parsing/haskell_parsec_basics/004_parsec_combining.hs
> :load 02_parsing/haskell_parsec_basics/004_parsec_combining.hs
> parse parseTuple "yod  1223"
--}
parseTuple :: Parsec.Parsec String () (String,String) -- input type, empty type, then output type
parseTuple = do
                letters <- Parsec.many1 Parsec.letter
                Parsec.spaces
                digits <- Parsec.many Parsec.digit
                return (letters, digits) 
                --return $ digits ++ letters

{--
Say we have a number of these letter/digit pairs, separated by some separator, for example a comma. 
In this case, we might want to parse them all into a list of tuples of the type seen above. 
Let's define another rule to parse our separator
--}            
mySeparator :: Parsec.Parsec String () ()
mySeparator = do
                Parsec.spaces
                Parsec.char ','
                Parsec.spaces
                --return () -- Optional as Parsec.spaces returns () 

mySeparatorSilentBind :: Parsec.Parsec String () ()
mySeparatorSilentBind = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces
                
parserAListTuples :: Parsec.Parsec String () [(String,String)]
parserAListTuples = Parsec.many $ do
                                    pair <- parseTuple 
                                    Parsec.many $ mySeparator
                                    return pair

-- desugared parserAListTuples * still seems to be 'magically' passing the String along..
myPairs :: Parsec.Parsec String () [(String,String)]
myPairs = Parsec.many (parseTuple >>= \pair -> mySeparator >> return pair)

parserTupAndSeparator :: Parsec.Parsec String () (String,String)
parserTupAndSeparator = do
                          tup <- parseTuple 
                          mySeparator
                          return tup                                    

-- TODO: why does this require a separator at the end?
parserListTupAndSeparator :: Parsec.Parsec String () [(String,String)]                        
parserListTupAndSeparator = Parsec.many1 $ parserTupAndSeparator    
    
{--
    Parsec.endBy - built in helper for the above
        Takes two arguments, a rule to parse items, and a rule to parse separators. Parsec.endBy 
--}    
myPairsEndBy :: Parsec.Parsec String () [(String,String)]
myPairsEndBy = Parsec.endBy parseTuple mySeparator
    
-- Parsec.sepBy - same as endBy, but doesn't expect the final separator
-- parse myPairsSepBy "hello 1, bye 2"
myPairsSepBy :: Parsec.Parsec String () [(String,String)]
myPairsSepBy = Parsec.sepBy parseTuple mySeparator
    
-- This seems like something for applicative (pairSep,pairend,combine)    
-- pairs2
sepByOrEndBy :: Parsec.Parsec String () [(String,String)]
sepByOrEndBy =  Parsec.many $ do
                                pairs <- parseTuple
                                Parsec.eof <|> mySeparator -- <|> Parsec.spaces: if we add this, it'll only consume the first tup
                                return pairs
    
{--
    Parsec.choice and <|> for matching one of multiple rules:
        Using Parsec.choice or the shorthand infix operator <|>
        (also in Control.Applicative)
        the first rule in the sequence to consume SOME input
--}
consumeFirstHThenChoice :: Either Parsec.ParseError String
consumeFirstHThenChoice = parse (Parsec.char 'h' >> (Parsec.string "ello" <|> Parsec.string "owdy")) "howdy"

-- playing around with type alias's 
type StringToStringParser = Parsec.Parsec String () String 
type MaybeParse2String = Either Parsec.ParseError String

-- now we want to save the 'h'
parserHthenChoice :: StringToStringParser

--  > parse parserHthenChoice "Hello"
parserHthenChoice = do
                     first <- Parsec.char 'H'
                     rest <- Parsec.string "ello" <|> Parsec.string "owdy"
                     return $ first : rest -- why does (first : rest) not work? 'c' : [char] .. didn't work the first time had to [first] ++ rest

{-- 
    Parsec.try
    it catches any failure and rewinds us.
--}

helloOrHowdy2 :: MaybeParse2String
helloOrHowdy2 = parse (Parsec.try (Parsec.string "howdy") <|> Parsec.string "hello") "hello"

-- parse parserRuleHelloOrHowdy2 "hello"
parserRuleHelloOrHowdy2 :: Parsec.Parsec String () String 
parserRuleHelloOrHowdy2 = do
                            x <- Parsec.try (Parsec.string "howdy") <|> Parsec.string "hello"
                            return x
                            
-- Creating your own error messages with <?>
import System.Environment
import qualified Text.Parsec as Parsec
--import Text.Parsec ((<|>))
import Text.Parsec ((<?>))
import Control.Applicative 

-- helper parsing function
-- without explicit type : Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse :: Parsec.Parsec String () a -> String -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(source)" text

{--
    http://unbui.lt/#!/post/haskell-parsec-basics
    
    Applicatives: <|> <?> 
        1. custom error handlers with <?>
        
    > ghci
    > :load 02_parsing/haskell_parsec_basics/005_parsec_custom_error_and_applicatives
    > paserWithDefaultError
    > paserWithCustomError
    
    -- parser with custom error using <?> Applicative combinator and try
    > parseHelloOrHowdy "hello" | Hello" | "howdy" | "Howdy"
    > parseHelloOrHowdy "wrongstring"
--}

-- Custom Error Handlers
-- "<?> allows you to attach a custom error to any rule quite simply."
-- <?> has the lowest precedence possible, everything else is evalutated first.

paserWithDefaultError :: Either Parsec.ParseError String
paserWithDefaultError = parse (Parsec.string "hello") "wrongstring"

paserWithCustomError :: Either Parsec.ParseError String
paserWithCustomError = parse (Parsec.string "hello" <?> ">.< not your lucky day") "wrongstring"


{--
    Q. How would <?> behave with <|>?
       rule1 <|> rule1 <|> rule3 <?> "oops!"
    
    think about
    * <?> has the precedence possible.
    * <|> fail if they cannot consume any input
    * what if ruleX starts consuming input and then fails? How should you handle the error?
    
    test it out:
        
--}

type MaybeStringParse = Either Parsec.ParseError String

parseChainWithDefaultError :: Either Parsec.ParseError String
parseChainWithDefaultError = parse (Parsec.string "try me" <|> Parsec.string "sneaky frist match") "somewrongstring"

noMatchWithWeakCustomError :: Either Parsec.ParseError String
noMatchWithWeakCustomError = parse (Parsec.string "try me" <|> Parsec.string "nope" <?> ">.< sorry 'bout that") "somewrongstring"

partialMatch_WithWeakCustomError :: Either Parsec.ParseError String
partialMatch_WithWeakCustomError = parse (Parsec.string "try me" <|> Parsec.string "sneaky frist match" <?> ">.< sorry 'bout that") "somewrongstring"

-- better error handlers

strongCustomError :: MaybeStringParse
strongCustomError = parse (Parsec.try (Parsec.string "try me" <|> Parsec.string "sneaky frist match") <?> ">.< didn't turn up a match." ) "somewrongstring"

-- let's break it down
parserHowdyOrHello :: Parsec.Parsec String () String 
parserHowdyOrHello = do
                       first <- (Parsec.char 'h'<|> Parsec.char 'H')
                       rest <- Parsec.string "owdy" <|> Parsec.string "ello"
                       return (first : rest)

parserHandleErrors :: Parsec.Parsec String () String
parserHandleErrors = Parsec.try parserHowdyOrHello <?> "sorry no match, but at least I told you."
            
parseHelloOrHowdy :: String -> Either Parsec.ParseError String
parseHelloOrHowdy text = parse parserHandleErrors text                

{-- 
 note: "This is not recommended for more significant rules as it would replace precise error messages 
        from sub rules with some general and less helpful error. However, when building small rules 
        it can be more descriptive to provide your own error over those Parsec provides."
--}

{--
    A: 
    "Attaching a new error message to the end of a chain of rules created with <|> for instance will result 
    in that error being used if all of the rules fail without consuming any input, as then the rule generated 
    from the <|> chain has failed. As soon as a rule consumes input, it becomes up to the error reporting of 
    that rule to describe future failure (unless of course a try block surrounds the rule, which suppresses 
    error messages from it). This basic example illustrates the fact:"
    
--}    


-- Applicative functions for more ...
-- "Applicative functions often make code shorter, since they are all about being point-free, 
--  that is, not making explicit references to the variables being passed around."

-- concise parsing

-- back to first parser parse letters and digits
parserLetterDigitTup :: Parsec.Parsec String () (String,String)
parserLetterDigitTup = do
                        letters <- Parsec.many1 Parsec.letter
                        Parsec.spaces
                        digits <- Parsec.many1 Parsec.digit
                        return (letters, digits)

parserLetterDigitTupList :: Parsec.Parsec String () [(String,String)]
parserLetterDigitTupList = Parsec.many1 $ do
                                            tup <- parserLetterDigitTup 
                                            Parsec.spaces
                                            return tup

paserLettersAndDigits :: String -> Either Parsec.ParseError [(String,String)]
paserLettersAndDigits text = parse parserLetterDigitTupList text

-- Now Applicative Style 1 - 3 are the same, but written differently
-- (<*>) :: f (a -> b) -> f a -> f b
-- (*>) ::  f a -> f b -> f b                        Sequence actions, discarding the value of the first argument.
-- (<|>) :: f a -> f a -> f a       monoid op?
-- (<$>) :: Functor f =>     (a -> b) -> f a -> f b
-- liftA :: Applicative f => (a -> b) -> f a -> f b 
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c   -- lift a binary function

-- note: putting Parsec.many1(Parsec.spaces *> Parsec.digit) only allows for string (let digit) input, and nothing trailing.

parserTup_App1 :: Parsec.Parsec String () (String,String)
parserTup_App1 = (,) <$> Parsec.many1 Parsec.letter <*>  (Parsec.spaces *> Parsec.many1 Parsec.digit)

parserTup_App2 :: Parsec.Parsec String () (String,String)
parserTup_App2 = liftA (,) (Parsec.many1 Parsec.letter) <*> (Parsec.spaces *> Parsec.many1 Parsec.digit)

parserTup_App3 :: Parsec.Parsec String () (String,String)
parserTup_App3 = liftA2 (,) (Parsec.many1 Parsec.letter) (Parsec.spaces *> Parsec.many1 Parsec.digit)

-- or even (swapping *> for the more familiar >>):
parserTup_App4 :: Parsec.Parsec String () (String,String)
parserTup_App4 = liftA2 (,) (Parsec.many1 Parsec.letter) (Parsec.spaces >> Parsec.many1 Parsec.digit)

    
    
-- <$> and <*>


-- liftAx

-- <* and *>

-- <$


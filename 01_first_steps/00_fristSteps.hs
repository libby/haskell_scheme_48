module Main where 
import System.Environment 
-- System.Environment  https://downloads.haskell.org/~ghc/6.4/docs/html/libraries/base/System.Environment.html#v%3AgetArgs
-- hold functions such as getArgs :: IO [String], getEnv :: String -> IO String, getProgName :: IO String

-- Every Haskell program begins with an action call main in a module named Main.
-- module names are always capitalized, definitions always uncapitalized.
-- monad - "internal values may change..., but the "pipe" (the value-propagation mechanism) Stays the same."
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps
-- compile: ghc -o srcfiles/00_fristSteps 01_first_steps/00_fristSteps.hs
-- run:     srcfiles/00_fristSteps "coffee"

main :: IO() -- type def is optional,  "extra info" IO actions to be performed
             -- "action" each potentially acting on the passed along basic values
main = do
        args <- getArgs -- reads command line args, passes along as a list of Strings.
        putStrLn("Hello," ++ args !! 0) -- !! 0 index(0) takes a String and creates an "action" that writes this String to the console.
        

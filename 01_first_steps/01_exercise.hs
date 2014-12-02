module Main where 
import System.Environment

-- Change the program so it reads two arguments from the command line, and prints out a message using both of them
-- compile: ghc -o srcfiles/01_exercise  01_first_steps/01_exercise.hs
-- run:     srcfiles/01_exercise "Simon" "Peyton-Jones"

main :: IO() 
main = do
        args <- getArgs-- reads command line args, passes along as a list of Strings.
        let arg1 = (args !! 0) -- cant' do this because this requires type IO[String]
        let arg2 = (args !! 1)
        putStrLn("First say hello to, " ++ arg1  ++ " then to " ++ arg2) -- !! 0 index(0) takes a String and creates an "action" that writes this String to the console.

        

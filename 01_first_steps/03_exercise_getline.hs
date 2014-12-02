module Main where
import System.Environment
-- getLine is an IO action that reads a line from the console and returns it as a string. 
-- Change the program so it prompts for a name, reads the name, and then prints that instead of the command line value
-- compile: ghc -o srcfiles/03_exercise_getline 01_first_steps/03_exercise_getline.hs
-- run:     srcfiles/03_exercise_getline   // TODO how do you enter input from the command line to a script prompt? << "Erik Meijer"

main :: IO()
main = do
        putStrLn("Hello, please enter your name: ")
        name <- getLine
        putStrLn("Good job! Here is your name back: " ++ name)
module Main where
import System.Environment

main :: IO()
main = do
        putStrLn ("Please enter a name: ")
        name <- getLine
        putStrLn("Your name happens to be: " ++ name)
        
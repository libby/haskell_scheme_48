module Main where
import System.Environment
 
main :: IO ()
main = do
	args <- getArgs 
	let x = (read (args !! 0) ) :: Int
	let y = (read (args !! 1) ) :: Int
	let res = x + y
	putStrLn (show res)

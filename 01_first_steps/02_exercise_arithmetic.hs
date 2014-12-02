module Main where 
import System.Environment (getArgs)
-- https://github.com/dstcruz/Write-Yourself-A-Scheme-In-48-Hours/blob/master/ch02/parsing/exercise_4.hs
-- Change the program so it performs a simple arithmetic operation on the two arguments and prints out the result. 
-- You can use read to convert a string  to a number, and show to convert a number back into a string. 
-- Play around with different operations.
-- compile: ghc -o srcfiles/02_exercise_arithmetic  01_first_steps/02_exercise_arithmetic.hs
-- run:     srcfiles/02_exercise_arithmetic 10 6

main :: IO() 
main = do -- getArgs https://downloads.haskell.org/~ghc/6.4/docs/html/libraries/base/System.Environment.html#v%3AgetArgs
        args <- getArgs -- :: IO [String]
        
        -- read :: Read a => String -> a
        -- so ... Read Integer => String -> Integer
        let res1 = (read $ args!!0) + (read $ args!!1 ) -- so is this implicitly cast?
        
        -- res2 get the arguments separately
        let arg1 = (read $ args!!0) :: Float -- b/c we aren't giving any other hints, i.e. '+' ing right away, we need to tell ghc what type of Read a we want, i.e. :: Float
        let arg2 = ( read (args!!1) ) :: Float -- Int, Integer, etc. as they all have instance Read [data Integer :: * ... Read Integer]
        let res2 = arg1 + arg2
        
        -- playing around with res1 
        print(res1) -- implicitly? wraps the value in a show -> String
        print(show res1) -- will be a String type
        print(show res1 ++ " show res1")
        print("show res1 " ++ show res1)

        -- playing around with res2  
        putStrLn("res2: " ++ show res2)
        putStrLn(show $ (read $ args!!0) + (read $ args!!1 ) ) 
        
        -- do it all one liner
        print((read $ args!!0) + (read $ args!!1 ) )  --https://www.haskell.org/hoogle/?hoogle=print
        

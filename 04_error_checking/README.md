## [Error Checking and Exception](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Error_Checking_and_Exceptions)

`Control.Monad.Error` and reporting on Error when they first occur

## Steps 
1. Add `data LispError` and  `instance Show LispError`

2. Make "ErrorType" into instance of Error

3. Type that may thrown an `Error`
"Type constructors are curried just like functions, and can also be partially applied.  A full type would be 
`Either LispError Integer` or `Either LispError LispVal`, but we want to say `ThrowsError LispVal` and so on. 
We only partially apply Either to LispError, creating a type constructor ThrowsError that we can use on any data type."

```haskell

	-- represents a Type that may throw an Error
	type ThrowsError = Either LispError
```

`Either` Monad
[`throwError`](http://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Error-Class.html#v:throwError)
[`catchError`](http://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Error-Class.html#v:catchError)


4. Finally, we need to change our main function to use this whole big error monad.

```haskell
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
```
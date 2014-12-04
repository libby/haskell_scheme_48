# Playing around with some Haskell :mouse:

## Notes where I got confused 
data type of [ ParsecT](http://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Prim.html#t:ParsecT)

```haskell
data ParsecT s u m a 
```
so here 's u m a' are any types, like generics in Scala/Java :bento:

```scala
trait Parsec[S,U,M,A]
```

```
(<|>) :: Monad m => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
```

**Here you are constraining m to the type Monad. It helps to read the types from right to left:**
> <|> goesParsecT takes type parameters s u m a, 

Functions in Haskell are automatically curried, i.e if a funct
```scala
 val addTwoNums: Int Int => Int = (x, y) => x + y
```
Enter Haskell REPL `ghci`
```haskell
 Prelude> let addTwoNums x y = x + y :: Integer -> Integer 	
 Prelude> :t addTwoNums
 addTwoNums :: Num a => a -> a -> a
 Prelude> let incNum = addTwoNums 1
 Prelude> :t incNum
 incNum :: Integer -> Integer
 Prelude> addTwoNums 3 3
 6
 Prelude> incNum 3
 4
```
In Haskell, like in Scala, declaring the type is optional
```haskell
 addTwoNums :: Int -> Int  //optionally declare the type when not inside an .hs file
 addTwoNums x y = x + y
```

## Ways to the monad and monad and monad... :scream_cat:
### Combining monads

1. the do sugar, mmmm :shaved_ice: 

// a stream seems to be passed implicitly into this 'monad'
```haskell
do 
  x <- many (noneOf "\"") 
return x
```
desugars to ...
-- many (noneOf "") >>= \x -> ...

2. `>>` for chaining without passing along the internal value, will short circuit when one monad in the chain fails.
```haskell
  monadA >> monadB >> monadC // if one fails, then the chain stops and returns the `failed` monad representation.
```

3. `>>=` for passing the internal value on one monad to the next monad in the chain, like `flatMap` in Scala.
```haskell
  monadA >>= \a -> (nextMonad a) >>= \b -> (nextMonad b) -> combineThemAll a b c
```

4. The `$` is like `()` and is used for passing arguments to a function it has the lowest precedence and is right associative. 
```haskell
   Prelude> incNum $ 3
```

// a where ParsecT is a type with a constrain on 'a' that it must be a Char see: data ParsecT s u m a  
letter :: Stream s m Char => ParsecT s u m Char

Haskell allows you to make these "anonymous types"

instance Functor (ParsecT s u m) where
	fmap f p = parsecMap f p
are instances like scala implicit type classes?
and like that! Found the perfect [blog post](http://www.cakesolutions.net/teamblogs/2012/10/04/typeclasses-in-scala-and-haskell)
"Typeclasses define behaviour for a particular type and typeclass instances implement that behaviour. 
In this sense, typeclasses are interfaces and typeclass instances are implementations of those interfaces. You do not instantiate the implementations, the compiler does, and it does so by matching the types"

# Useful things to learn quick
 - cabal is the the package manager for Haskel
 - Hoogle
 - System.io.unsafeIO! 
 - debug, trace.
 - data types, instance (implicit TC scala?)
 - monadic chaining:

```haskell
	 do
	   x <- myMonad
	 return x
```	
 syntatic sugar...desugars to `>>=` like Scala 
 `>>=` when you need to pass along the value
 `>>` when you don't need to pass along a value

---
	
# Fun facts
 - Haskell (Glasgow '90s) hence ghc [Glasgow Haskell compiler] vs ML (University of Edinburgh 1973 Robert Milner )

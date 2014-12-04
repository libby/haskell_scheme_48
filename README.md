# Just playing around with some Haskell

# Notes where I got confused http://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Prim.html#t:ParsecT
* see: data ParsecT s u m a 
  so here 's u m a' are any types

combining monads
(<|>) :: Monad m => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
here you are constraining m to the type Monad

// a stream seems to be passed implicitly into this 'monad'
do 
  x <- many (noneOf "\"") 
desugars to
-- many (noneOf "") >>= \x -> ...

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
	 do
	   x <- myMonad
	 return
	 syntatic sugar...desugars to >>=  like scala 
	 >>= when you need to pass along the value
	 >> when you don't need to pass along a value
	
# Fun facts
 - Haskell (Glasgow '90s) hence ghc [Glasgow Haskell compiler] vs ML (University of Edinburgh 1973 Robert Milner )

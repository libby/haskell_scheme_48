### [An introduction to parsing text in Haskell with Parsec](http://unbui.lt/#!/post/haskell-parsec-basics)
The `haskell_parsec_basic` directory playground is what happened when I went through the above tutorial.
While trying out 48 hour
### minor mistakes in the tutorial
* import Text.Parsec (<|>) should be import Text.Parsec ((<|>))

### Quick list of things I learned going through this tutorial
* loading file in the REPL
```
    > ghci
    Prelude> :load file_name
```
* `instances` in Haskell are like Scala type classes. Haskell data types are the struct and the instance determine what addition behavior type has, i.e. [data Integer](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#t:Integer)

```haskell
data Integer :: *

instances
Eq Integer
Read Integer
Show Integer

```
The instances add behavior, i.e. `Show Integer` Haskell instance is like `toString` in Scala/Java if and instance of `Show YourType` is defined then the object can be output to standard out. 
```
Prelude> show 1
"1"
```
These felt like scala implicits to me, and a quick google search found [Typeclasses in Scala and Haskell](http://www.cakesolutions.net/teamblogs/2012/10/04/typeclasses-in-scala-and-haskell) :sparkles:

> not sure about data instance Read Integer...sort of feel like scala implicits to me.

* One line comment 
```haskell 
-- comment
```
multiple line comment 
```haskell
{-- 
	this comment 
	can span multiple lines
--}
```
* Lots and lots about Parsec

### Quick run

004 
``` 
> ghci
Prelude> :load 02_parsing/haskell_parsec_basics/004_parsec_combining.hs
Prelude> parse parseTuple "yod  1223"
```
005 working with applicatives and custom errors 
``` 
> ghci
Prelude> :load 02_parsing/haskell_parsec_basics/005_parsec_custom_error_and_applicatives
Prelude> paserWithDefaultError
Prelude> paserWithCustomError
Prelude> parseHelloOrHowdy "hello" | Hello" | "howdy" | "Howdy"
Prelude> parseHelloOrHowdy "wrongstring"
```

### Quotes from the tutorial I like

> all building up to one flow, then you can pass data in and the functions / monads are compassable as long as they have the same shape. 
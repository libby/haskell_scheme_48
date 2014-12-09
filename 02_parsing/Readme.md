# Helpful Links
- [parsec lib github](https://github.com/aslatter/parsec)
- [parsec lib documentation on Hackage](https://hackage.haskell.org/package/parsec)
- [An Intro to Parsing Text In Haskell With Parsec](http://unbui.lt/#!/post/haskell-parsec-basics)

Practicing in here, so lots of redundant typing.



-- or even (swapping *> for the more familiar >>):
myParserApp :: Parsec.Parsec String () (String,String)
myParserApp2 = liftA2 (,) (Parsec.many1 Parsec.letter) (Parsec.spaces >> Parsec.many1 Parsec.digit)

## Questions

* Why does this give a stack overflow.

```
  > :load 02_parsing/00_write_a_simple_parser.hs
  > parse parseString "text" "\"asdf\""
    Right *** Exception: stack overflow
```

case parse (parseString) "lisp" "\"abc\"" of |
                    Left err -> "didn't find value " ++ show err
                    Right val -> "found value"


## Find out where files are 
* check out the parsec [github project](https://github.com/aslatter/parsec)

> :info many

i.e.
```
info noneOf
noneOf ::
  Text.Parsec.Prim.Stream s m Char =>
  [Char] -> Text.Parsec.Prim.ParsecT s u m Char
  	-- Defined in `Text.Parsec.Char
```
Then look up the definition in Text.Parsec.Char

```haskell
noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
```

> :t Parsec

## Built in Functions read, show, 
`Read` and `Show` are Typeclasses that are defined on many data types, they add additional behavior.

## Read Typeclass: convert a String to the datatype
"First, we use the built-in function read to convert that string into a number."
Read is a Typeclass that can be defined on any type, see ...
instance Read Integer 
with the function read :: String -> Integer 
Read is a Typeclass as is Show which deals with showing a 

## Show Typeclass: convert the datatype to a string, very useful for outputting results. Like toSting.

```haskell
instance Show Integer 
putStrln(show 1)
```

## to string
* instance Show Datatype
* [first] ++ rest // `++` concat two lists, i.e. [Char] ++ String or equivalently [Char] ++ [Char] as type String = [Char]
* first : rest // append first to the front of list.


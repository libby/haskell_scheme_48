## [Evaluation Part 2](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Evaluation,_Part_2)

### Additional Primitives: Partial Application - Adding control structures: Booleans, Conditionals
* Binary boolean ops
* Adding more primitives for boolean os `>, <, <=, >=, &&`, etc.

## Conditionals: Pattern Matching 2
* Add if conditional - pattern matching on 4 elem list List(Atom["if", pred, conseq, alt])


## List Primitives: car, cdr, and cons
* think of them in terms of their effect on printed S-expressions 

1. (car '(a b c)) = a   => `List("car", "quote", List)`
2. (car '(a)) = a
3. (car '(a b . c)) = a
4. (car 'a) = error – not a list
5. (car 'a 'b) = error – car only takes one argument

## Implement `eqv?`
## Scheme offers three levels of equivalence predicates: `eq?`, `eqv?`, and `equal?`
`eq?`, `eqv?` - recognize two items as the same if they print the same

## equal? and Weak Typing: Heterogenous Lists
* "ignores differences in the type tags and only tests if two values can be interpreted the same. For example, (eqv? 2 "2") = #f, yet we'd like (equal? 2 "2") = #t"

* The let (Bool x) = eqvEquals in x is a quick way of extracting a value from an algebraic type: it pattern matches Bool x against the eqvEquals value, and then returns x

## Compiler flags for `Existential Types` `– -fglasgow-exts` or add `{-# LANGUAGE ExistentialQuantification #-}` to the beginning of code.
-Xfoo can be replaced by the pragma {-# LANGUAGE foo #-} inside the source file

unpacking with AnyUnpacker 
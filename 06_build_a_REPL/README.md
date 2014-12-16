## [Building A REPL](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL)
###  execute multiple statements without exiting the program. ... build a read-eval-print loop.

* All about IO
-  build a system that can execute multiple statements without exiting the program.
-  read-eval-print loop.

```haskell
	-- loop '_' in Haskell is a typical naming convention for monadic functions
	until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
	
```
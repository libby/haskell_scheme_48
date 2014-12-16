## [Variables And Assignment](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Adding_Variables_and_Assignment)

### The State Monad
1. [data State s a](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base/Control-Monad-ST.html)']
   creates a stateful computation that can be executed as a unit, without the state escaping to the rest of the program. 

2. [Data.IORef](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base/Data-IORef.html) -A mutable variable in the IO monad
   Allows you to use stateful variables within the IO monad.

Haskell provides a mechanism known as *monad transformers* that lets you combine the functionality of multiple monads
[`ErrorT`](https://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Error.html) :
```haskell 
	newtype ErrorT e m a 
```
> The ErrorT Monad structure is parameterized over two things:
>
>    e - The error type.
>    m - The inner monad.

> The error monad transformer. It can be used to add error handling to other monads. 

## About the IORef
* IORefs can only be used inside the IO monad.


## Dealing with two Monads once the IORef environment is added: env monad, and error monad.

*monad transformers* let you combine the functionality of multiple monads. `ErrorT` is a *monad transformer*
```haskell
  -- add error handling to other monads. 
  -- e The error type, m the inner monad
  newtype ErrorT e m a 
  
 -- constructor
  runErrorT :: m (Either e a)

  -- example wraps an IO action monad
  type ErrorWithIO e a = ErrorT e IO a => ErrorT(IO (Either e a)) 

````
> Methods in typeclasses resolve based on the type of the expression, so throwError and return (members of MonadError and Monad, respectively) take on their IOThrowsError definitions.

> we want to change the variable instead of just reading it. The writeIORef action provides a means for this, but takes its arguments in the wrong order (ref -> value instead of value -> ref). So we use the built-in function flip to switch the arguments of writeIORef around, and then pass it the value. Finally, we return the value we just set, for convenience

`flip` cool!
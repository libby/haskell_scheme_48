## [Creating IO Primatives](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Creating_IO_Primitives)

## Time To Communicate With The Outside World

## Things TODO:
* LispVal constructor to support IO functions
* LispVal constructor Port for Scheme data type [port](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.6.1)
> A Handle is basically the Haskell notion of a port: it's an opaque data type, returned from openFile and similar IO actions, that you can read and write to.


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args  -- difference?
applyProc (func : args)     = apply func args

## Pattern Matching
makePort mode [String filename]
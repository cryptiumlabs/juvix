Bespoke compilation path only, no interaction nets.

- Stack tracking
- Calling conventions
- Clear stack when variable won't be used again
- Encode all natives as built-in functions, dependently-typed where applicable

Closures:
- Scan the body of the function for referenced variables
- Compile the function as a lambda on all variables and a tuple of the referenced variables followed by `APPLY` for uniform typing
- Compile applications as an unpacking of the referenced variables and then `EXEC`

Outstanding questions:
- How to pass higher-level datatypes through core for natural representation? Add them to primitive values & require higher-level language to create them? Do we need special treatment (= overrides for standard library) or can we do this structurally (a la GHC.Generics)?
- Same question for case statements.

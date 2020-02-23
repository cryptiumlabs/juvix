Holes stand for parts of terms which have not yet been defined, but which the typechecker can still reason about in some limited capacity by their context.

Similarly to Idris, holes can be created by prefixing a question mark to an identifier:

```
term : 1 Int
term = ?term_rhs
```

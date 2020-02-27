Similarly to Idris or Agda, arguments can be omitted in type annotations & function calls, and their values will be inferred. When necessary, implicit arguments can be referenced, captured, or passed with `{}` syntax:

```
id : { a : ∗_i } → a → a
id x = x
```

There are no syntactic restrictions on usage of implicit arguments. If the compiler cannot infer a value, an error will be thrown.

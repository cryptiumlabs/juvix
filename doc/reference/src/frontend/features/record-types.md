Record types can be defined with the `record` keyword:

```
record R : Γ → ∗_i where
  constructor RC
  x : A
  y : B x
  z : C x y
```

This declaration creates a type `R`, constructor `RC`, and accessors `x`, `y`, and `z`. Note that the type of `y` depends on `x`, and the type of `z` depends on `x` and `y`.
`Δ` are the parameters, scoped over the types of the fields and the type of the constructor.

Record types & accessor functions desugar to dependent pairs (although the frontend typechecker treats separate record declarations with the same fields as separate types).

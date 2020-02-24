Similary to Idris & Agda, datatypes are defined with inductive family `data` declarations:

```
data D Δ : Γ → ∗_i where
  c_1 : Θ_1 → D Δ t_1
  c_n : Θ_n → D Δ t_n
```

- `D` is the datatype family.
- `c_1` through `c_n` are the constructors, with given types.
- `t_1` through `t_n` (vectors) are the indices for the constructor return types.
- `Δ` are the parameters, scoped over the types of the constructors, which must be constant in their targets.
- `Γ` are the indices, which can vary over the type of the constructor targets.

In general, constructor types can reference `D` inductively.
Different parameterisations of core may represent datatypes in different ways, and might impose
restrictions such as positivity (`D` can only occur to the left of an even number of arrows in the constructor type)
or strict positivity (`D` cannot occur to the left of an arrow anywhere in the constructor type).

Mutually inductive-recursive definitions will be supported at a later date.

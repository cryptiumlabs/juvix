Primarily inspired by the GRIN [@graph-reduction-intermediate-notation] paper & implementation.

(todo: determine which of these are rendered unnecessary by interaction net evaluation; keep it as simple as possible)

(todo: some of these need to be applied at a lower layer and only when terms are compiled to custom rewrite rules via the bespoke path)

See [this example](https://nbviewer.jupyter.org/github/grin-compiler/grin/blob/master/papers/boquist.pdf#page=317).

Possible transformations:

- vectorisation
- case simplification
- split fetch operation
- right hoist fetch operation
- register introduction
- evaluated case elimination
- trivial case elimination
- sparse case optimisation
- update elimination
- copy propagation
- late inlining
- generalised unboxing
- arity raising
- case copy propagation
- case hoisting
- whnf update elimination
- common sub-expression elimination
- constant propagation
- dead function elimination
- dead variable elimination
- dead parameter elimination

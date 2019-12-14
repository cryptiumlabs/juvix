Open questions
- How to structure the core dialect? Will need to eventually turn into an arithmetic circuit with addition gates, multiplication gates, and an element type (usually a field element) which is converted into a rank-1 constraint system. The circuit encodes a relation between the input & output, i.e. `circuit :: Input -> Output -> Bool`
- Input & output types must desugar to vectors of the element type with fixed lengths.
- Higher-order functions must be reducible at compile time to relations.
- Probably will eventually need Rust bindings to the prover/verifier code ([ref](https://blog.mgattozzi.dev/haskell-rust/), [ref](http://hackage.haskell.org/package/curryrs)). This doesn't matter so much if our primary goal is to output a circuit description in some standard format and 'glue code' for integration into smart contracts, though.

Inspirations: [snarky](https://github.com/o1-labs/snarky), [genSTARK](https://github.com/GuildOfWeavers/genSTARK), [ZoKrates](https://github.com/zokrates/zokrates).

We should only do this if it will actually be useful (compared to existing DSLs). I think it would be useful if and only if:
- We can implement a higher level of abstraction without sacrificing performance thanks to the powerful typesystem & usage information, and possible optimisation layers
- We can nicely integrate code running in zero-knowledge with regular smart contract code in a way that is very convenient for contract authors, automates all the data conversion tedium, and allows for proofs across multi-backend code (ref https://github.com/cryptiumlabs/juvix/issues/157#issuecomment-552046922)
- We can output performant code (circuit descriptions to be executed on dedicated provers/verifiers and smart contracts as appropriate) that can be used in production.

Higher-order functions should be possible if we only operate on closed terms & fully evaluate under lambdas - in particular consider the following case (of a case statement):

```
relation :: Input -> Output -> Bool
relation input output =
  (case input of
     1 -> (==) 2)
     2 -> (==) 3) output
```

If we evaluate under the case (which will desugar to a lambda), we should end up with:

```
relation :: Input -> Output -> Bool
relation input output =
  (input == 1 && output == 2) ||
  (input == 2 && output == 3)
```

which is just fine.

Note that to do this we need some prior knowledge of the type - namely a relation - so that we know that the case statement should turn into an `OR` on the relations of the scrutinee, evaluated clauses & bodies.

Also note that it would be nice to avoid evaluating the scrutinee multiple times for multiple case branches, this should be achievable as an intermediary circuit node.

(to encode the OR in the circuit - `or x y -> x + y - (x * y) = 1`)

Maybe we could also try an evaluation monad which keeps intermediary results (basically 'allocating memory' as necessary) and can output a circuit representation. Might be convenient for avoiding duplication of intermediary evaluations. Advantages: structurally simpler and captures the idiomatic flow better.

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

**Example high-level usage example (approximate)**

```
contractFunction :: [Input] -> [Output] -> m Result
contractFunction inputs outputs = do
  let verifier = inSnark $ \inp out -> sumAll inp == sumAll out
  unless (verifier inputs outputs) (throw UnequalSum)
  sub inputs
  add outputs
  pure Success
```

Note in particular that - subject to [backend abstraction](https://github.com/cryptiumlabs/juvix/issues/157) - code should be reusable, e.g. `sumAll` should be callable in both circuits & regular (e.g. Michelson) backends, assuming that the functions called therein (addition, equality) are supported on the backends in question

**Compilation questions**

We can assume that the compiler will operate on a closed term `rel :: x -> y -> Bool` (a relation) where `x` and `y` are either primitive types or ADTs (possibly inductive) composed of primitive types, and that all lambda functions will be inlined & fully evaluated at compile-time, so `rel` will necessarily be a two-argument function returning a boolean (with arbitrarily nested pattern-matching & fully applied primitive functions, e.g. and, or, add, mul, etc.). Calls to other functions - recursive functions must have compile-time fixed depth (e.g. folding for Merkle path verification) - will be fully inlined (at least for the initial version, later, e.g. with custom gate compilation or AIR, this might change, but it's easiest for now).

*Compiling ADTs*

At this level, inductive families should desugar/erase to simple algebraic datatypes.

Product types:
- Associated field elements (association tracked at compile-time)
- Figure out when sums (e.g. `(int, int)`) can be packed in one field element

Sum types:
- Tagged union (again, figure out how to pack this efficiently)

*Pattern matching*

Tagged equality matching (how to make this more efficient?) under an `OR` (note: need to be careful that this preserves the pattern-ordering semantics of the high level language, possibly this target may have some extra warnings or limitations here).

*Custom gate generation*

Investigate PLONK-style custom gates & compile-time generation. Needs more research.

*Workflow*

Proving/verifying keys generated separately? Prover code? Easy to solve later, I expect.

Captures will work only for values known at compile-time, otherwise they must be in inputs/outputs (unless we want to do some sort of "implicit output" thing, which is possible, could also be interesting - maybe we should go all the way and have only the private witness data be the input).

Datatype encoding
- Sum types
  - left | right
  - Size = `sizeof(tag) + max(sizeof(left), sizeof(right))`
    - tracked by the compiler
    - keep it in # of bits
    - this is just to determine how many field elements we need to use to represent
  - Pack tag & (left | right) into field elements
- Product types
  - `(fst, snd)`
  - Size = `sizeof(fst) + sizeof(snd)`
  - Pack (fst, snd) into field element
- Whether we can pack / how we pack depends on the actual types
- so e.g. (int, int, int) => packed into one field element
- e.g. (field, field) => needs two field elements (we have to track this through compilation)
  - compiler has to track where variables are in the circuit while compiling
- if x is (field, field), gets two input wires, compiler must track which is fst and which is snd, then when something is asserted e.g. `(fst x == 2) && (snd x == 3)` the compiler looks up which is fst and which is snd and hooks up the right wires to check this

pattern matching

```
  \x . \w . 
    case x of (xfst, xsnd) =>
      case w of (wfst, wnd) =>
        xfst == wsnd && xsnd == wfst
```

more complex pattern matching

```
  \x . \w .
    case x of
      L xi -> xi == 3
      R (xfst, xsnd) =>
        case w of
          (wfst, wnd) => xfst == wsnd && xsnd == wfst
```

This needs to turn into checking that either the tag of x is L and that `xi == 3`, or that the tag of `x` is `R` and that `xfst == wsnd && xsnd == wfst`.

packing things

- field element is in a group of some order (255 bit or something)
- uint64
- first 64 bits of the field element are first uint, second 64 bits are second uint, third 64 bits are third uint
- let's say we pack like this and then we want to "extract"

- `fst == 3 && snd == 4 && thd == 6`
- then we can just compose the other packed element = `(3, 4, 6)` and check equality directly

- `fst > 3 && snd < 4 && thd == 6`
- more complicated
- either we can extract, where we turn the packed field element into three field elements
  - assert that the bits are correct, basically
  - and then we can do the comparisons and and/or operations on the unpacked field elements
  - if we are doing a lot of extracting, it may not make sense to pack anymore, tbd
  - packing saves space in inputs & witness, but if we unpack a lot it costs constraints
  - whether this makes sense depends on how much unpacking we're doing and on the costs of the proof system (prover time, verifier time, proof size) w.r.t. input, witness size & constraints
  - we can worry about proof system specific optimisations later
- or we can try to compose some custom gate

- Context in the compilation pipeline for circuits to track datatype encodings
  - so that we are packing or unpacking variables we know how to do it

- plookup custom gate compilation

*What theoretical ingredients & practical architectural choices does Juvix make to realise these desiderata?*

*What changes are made from these prior systems, and why might one expect the solutions proposed to work?*

## Dependent types

### Basics

The first and most essential ingredient is that of *dependent types*: a typesystem expressive enough
to allow terms written in Juvix to express properties about the computation behaviour of other terms.

For example, the fairness of Tendermint proposer election has been [partially verified in Idris](https://github.com/cwgoes/tm-proposer-idris), which has the same kind of dependent typesystem:

```
fairlyProportional :
  (idA : ProposerId) -> (idB : ProposerId) ->
  (wA : ProposerWeight) -> (wB : ProposerWeight) ->
  (pA : ProposerPriority) -> (pB: ProposerPriority) ->
  (n : Nat) ->
  (wA >= 0 = True) -> (wB >= 0 = True) ->
  (abs(pA - pB) <= (wA + wB) = True) ->
  ((count idA (snd (incrementElectMany n ((idA, wA, pA), (idB, wB, pB)))))
      >= ((n * (wA / (wA + wB))) - 1) = True,
   (count idA (snd (incrementElectMany n ((idA, wA, pA), (idB, wB, pB)))))
      <= ((n * (wA / (wA + wB))) + 1) = True)
```

`incrementElectMany`, the proposer-election function, is the term whose behaviour we want to reason about,
and inhabiting the type of `fairlyProportional` with a valid term proves that it behaves in the fashion we intend.

In English, this proof could be read as "a validator, in a sequence of proposer elections where no other power changes take place,
proposes no fewer blocks than the total blocks in the epoch multiplied by its fraction of stake less one,
and proposes no more blocks than the total blocks in the epoch multiplied by its fraction of stake plus one".

### Comparisons to other techniques

Dependent types as implemented in Juvix satisfy all three dimensions of the [lambda cube](https://en.wikipedia.org/wiki/Lambda_cube):
terms can depend on types, types can depend on types, and types can depend on terms. In accordance
with the [Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence), types in Juvix can
express propositions, or properties of terms, and programs which inhabit those types serve as proofs of the propositions.

There are several different formal verification techniques, and they each have different trade-offs. Dependent types
are quite expressive, since the conversion rule is just beta reduction, and allow for property verification
in the same language, which removes the need for an error-prone translation between a specification and implementation.
They allow for progressive verification — contracts can initially be written "unverified", and proofs can be added
over time as the logic of an application-in-development solidifies. We have found that dependent types are elegant
and become reasonably intuitive with practice, but more tooling support will undoubtedly be necessary to smooth
the learning curve and ease some of the proof bureaucracy, which can be onerous at times.

### Proof-generation bureaucracy automation

Juvix's high-level syntax, compiler, and REPL utilise multiple tactics to minimise the bureaucracy of writing formal proofs of properties of terms. Generalised assisted graph search automates construction of proofs locatable in constrained search spaces, holes enable the developer to type, assert, and prototype now, then prove later when the model is finalised. Step-through tactics illuminate the inner de-sugaring & typechecking steps of the compiler to provide introspection & legibility.

### Composable proof ecosystem

As proofs of properties of Juvix terms are simply terms themselves, proofs can be exported & imported from libraries along with the code which will actually be executed. Proof interfaces for common data structures allow swapping out backend components of a higher-level module while retaining verified properties.

## Usage quantisation

The second theoretical ingredient Juvix brings to the table, used both to allow developers to
reason even more precisely about the behaviour of their programs and to ensure that using dependent
types for formal verification incurs no runtime cost, is usage quantisation, drawn from
the [Quantitative Type Theory paper](https://bentnib.org/quantitative-type-theory.pdf) and [extended by us](https://github.com/cryptiumlabs/juvix/issues/87).

Typing judgements in Juvix are annotated with precise usages:

$x_1 \overset{ρ_1}{:} S_1, \dots , x_n \overset{ρ_n}{:} S_n \vdash\ M \overset{σ}{:} T$

These usage annotations indicate how many times the subject of the judgement can be "computed with",
or used. This is a form of linear types, but much more expressive: usage annotations are numerical and
can depend on terms just like types can. Usage annotations are checked at compile-time for correctness.
Since proofs of properties need not be computed over and can be constructed in the zero-usage fragment,
Juvix can then erase proofs at compile time after checking them, ensuring that property verification
creates no runtime overhead. Detailed usage accounting also allows Juvix to avoid garbage collection,
optimise stack usage, and safely modify values in-place, in various situations depending on the types
and the granularity of usage annotations provided.

## Efficient execution

### Whole-program optimisation

Juvix takes advantage of the unique trade-offs of the smart contract use case — small source-code sizes and
infrequent deployment, which allow for longer compile times and wider optimisation searches — to enable
several kinds of whole-program optimisations. Whole-program optimisation encapsulates a collection of techniques & optimisation passes
rather than any specific one, drawn from multiple inspirations including [GRIN](https://github.com/grin-compiler/grin), [STOKE](https://github.com/StanfordPL/stoke) and [Morte](https://github.com/Gabriel439/Haskell-Morte-Library),
including compile-time evaluation, inlining, fusion, case simplification, common sub-expression elimination,
various dead code elimination checks, and rewriting provably equivalent terms.

### Interaction net evaluation

For certain backends, Juvix can evaluate lambda calculus terms using the abstract model
of interaction nets, as outlined by [Lamping, Asperti, and Guerrini](https://www.cambridge.org/vi/academic/subjects/computer-science/programming-languages-and-applied-logic/optimal-implementation-functional-programming-languages?format=HB) and inspired by [prior work](https://medium.com/@maiavictor/solving-the-mystery-behind-abstract-algorithms-magical-optimizations-144225164b07).
Interaction nets minimise the number of beta reductions by sharing evaluations, perform runtime fusion, support automatic parallelism, and handle
large closures efficiently. They come with some overhead, but this can be mitigated with [bespoke encoding](https://github.com/cryptiumlabs/juvix/issues/85)
— generating custom rewrite rules at compile-time — for small, performance-critical functions.

### Linear dependent types obviate garbage collection and ensure type erasure

The core type theory of Juvix combines linear & dependent types, extending prior research into the combination of the two paradigms with additional linear connectives & pragmatic essentials and instantiating usage quantisation over the natural numbers to provide maximally precise accounting. Dependent types enable the language to verify properties of its own terms in succinct proofs and open up a wide arena of compiler optimisations. Linear types obviate the need for garbage collection in both the optimal reduction & alternative direct subterm compilation paths, facilitate aggressive imperative optimisation transformations, and ensure that dependent types used to enforce properties but not needed at runtime are always erased by the compiler.

### Discrete-cost optimisation

Purpose-built for the smart contract use case, Juvix's optimiser requires a discrete instruction cost model of the underlying machine (likely a distributed ledger) which it can utilise to search through semantically equivalent instruction sequences and select the lowest by cost.

### Proofs become optimisations

The dependent type system of Juvix Core enables it to express arbitrary properties of its own terms, including equality of functions — proofs of which can be utilised by the optimiser to select between reducible expressions known to be semantically equivalent at compile time.

### Programmer-directed optimisations

Primitives are provided to allow developers to bypass the usual compilation pipeline and construct hand-optimised rewrite rules specialised to the underlying machine, optionally proving equivalence of their hand-optimised rewrite rules to the compiler-generated ones using a formalised interpreter for the machine model.

## Resource verification

Leveraging the dependent typechecker already in place, Juvix uses techniques from
[a 2008 POPL paper](http://www.cse.chalmers.se/~nad/publications/danielsson-popl2008.pdf) to check computational resource consumption
of terms without executing them by encapsulating computations in a cost monad, where
costs can depend on terms in the usual dependent fashion. This requires some annotations
by the developer, but tooling can be improved over time, and cost bounds can be added
in a progressive fashion just like proofs of properties. In the future, if ledgers elect
to integrate a limited version of the typechecker directly into their state machines, this
resource verification mechanism can be used to eliminate runtime gas metering entirely:
each contract would have a cost function, computable depending on arguments to the call,
and the gas (exactly computable) could be deducted at the beginning of the function prior
to execution. This resource verification model can also easily incorporate different denominations
of resources (compute, storage, etc.) by tracking several costs at once.

## Backend parameterisation

Juvix parameterises the type theory & core language over a set of primitive data types
and primitive values, which can include native data types such as strings, integers, or sets,
and native functions such as addition, subtraction, string concatenation, set membership, etc.
The language & typechecker can then be instantiated over a particular backend which provides
concrete sets of primitives and a primitive type-checking relation. Backends are integrated
into the language itself as special values, so that the standard library can nicely abstract
primitives with different underlying implementations into common typeclasses for numbers,
strings, etc. and resolve operators to the correct backend at compile time as instructed
by the developer. This will allow contract authors to write a single contract
in the frontend language and target different backends with compiler switches
or compile to more efficient upgraded versions of a virtual machine later on, with
minimal or no code changes, and also eases the path towards integrated typechecking
of applications with components on multiple backends (perhaps multiple ledgers and
a zkSNARK circuit, for example).

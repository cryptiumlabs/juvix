## Cost model

Currently tracked:

- Memory allocations
- Sequential rewrite steps
- Parallel rewrite steps
- Maximum graph size
- Final graph size

In the future we may want to track more granular memory operations (reads/writes in addition to allocations) and computations associated with rewrite rules (all negligible-cost with interaction combinators, but not with e.g. integer arithmetic).

Machine backends are expected to provide a discrete cost model which can be utilised by the optimiser.

## Accounting approaches

### Cost per VM instruction

Gas = cost per instruction on VM - e.g. EVM, Michelson.

1. Advantages
    1. Works, simple
1. Disadvantages
    1. Adds overhead - have to increment counter, check each instruction
    1. UX is pretty terrible - must estimate gas before sending tx, state could change
    1. (~) Cross-contract calls are dumb and hard to optimise (more a problem with using a low-level VM)

### Prior cost calculation

Calculate gas cost prior to execution.

1. Need: `cost :: (call, params) => Natural`
    1. Must be (far) cheaper to evaluate than just computing the function
    1. Must be verifiable (once) by the state machine for a particular contract, so need e.g. verified interpreter (for a VM)
    1. Might be worst-case cost, not tight bound (but should be able to make dependent on param values, so should be tight)

### Execute off-chain, verify on-chain in constant time

1. Constant gas cost + size of storage diffs
    1. User must do all execution (e.g. in ZK), verification is O(1) or user pays circuit size (easy enough) + proportional cost for state changes
    1. Bottleneck: prover time. TinyRAM runs at ~1Hz for the prover at the moment (old paper though). Maybe inets could be a bit more efficient.

## Monadic cost metering

I think we can utilise the basic monadic system in [Danielsson](http://www.cse.chalmers.se/~nad/publications/danielsson-popl2008.pdf)'s paper, adding in costs to particular rewrite rules in the evaluation semantics and ensuring that the reduction of an encapsulated term (the cost monad being erased) is bounded by the associated cost.

Some outstanding questions:

- Base primitives & functions will need to have non-unit costs
- How does optimal sharing change the necessary accounting (maybe not much, it already assumes call-by-need, but we need to think about what happens when thunks are copied)
- Can we add in discounts for parallelism (this would need to be understood by the compiler)?
- Can we automate or infer costs? The programmer overhead seems high.
- This doesn't really account for memory usage, which we may need to consider as well.

Additional thoughts after reading McCarthy 2016:

1. Propositional approach is neater, though we could also return 0-usage naturals in QTT.
1. It would be preferable to tie the monadic cost calculations directly to the cost semantics of the evaluator - in particular, we need to capture memory allocations, possibly bound the total graph size, and differentiate between sequential & parallel rewrite steps in some suitable fashion.
1. Allocations may not be unit cost
    1. For primitive data types, cost will depend on the size of the data type
1. Rewrite steps will not be unit cost
    1. Some may have primitive costs (built-in functions), bespoke-encoded rule costs must be calculated according to the term encoded into the bespoke rule
1. Costs assigned to computation & memory allocation must be sufficiently accurate, as a lower bound, to serve as a cost model for a contract on a public blockchain.
1. Cost inference would be nice in the future (the Coq library could perform some transforms, but I think it would be preferable to have explicit costs - just infer them where possible)
1. How best to deal with runtime fusion (e.g. $not . not . not . not$)? Not sure yet.

Example of the runtime fusion case:

```
true t f = t

false t f = f

not b t f = b f t
```

$not\ .\ not$ fuses once.

$not\ .\ not\ .\ not\ .\ not$ fuses like this twice in parallel, then once.

$log\ n$ in the general case.

whereas $not$ itself, naively applied $n$ times, would be linear complexity.

Another example: fast exponentiation by squaring.

Conclusions (tentative):

- Elaborator-based approach a la ZF*, with eventual editor support or the like for ergonomics.
- Parameterised costs for flexible machine targeting and easier upgrades.
- Think more about how to deal with runtime fusion, as it results from semantics, not syntax.
- Think more about explicit parallelism and how that will interact with the cost model.
- Think more about how QTT will interact with the cost model or could render inference easier.

## Machine-level optimisations

- Requires formalised VM semantics of particular machine model (in Juvix)
- User can prove semantic equivalence of machine instruction sequences
- Compiler can pick which instruction sequence is cheaper and compile to it (& pick differently in different cases)
- Must be tied into cost accounting

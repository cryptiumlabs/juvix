Target LLVM IR, with parallel (thread?) support.

Experiment with "unrolling" the basic interaction net evaluator case switch by some degree $k$, such that intermediate allocations can be optimised away at compile-time - code size will be $c ^ k$ with $c$ the number of branches, but that's still fine for a few levels of unrolling.

This transformation is trivially semantics-preserving in the single-thread case and no additional bookkeeping is necessary. In the multi-thread case, if two threads start reduction less than $k$ hops from each other (hop = wire between nodes), they may reduce overlapping primary pairs, which could be problematic. We could either attempt to eliminate this possibility at compile time, which will require a fair bit of knowledge about where parallel reduction will occur (but perhaps possible, especially with explicit annotations), or reduce optimistically in parallel, insert synchronisation points where necessary, and revert conflicting changes in a semantics-preserving way - an approach which is more complicated to reason about, but doesn't require compile-time knowledge of "distance" between threads rewriting the graph in parallel.

API:
- createNet : [Node] -> IO ()
- readNet : IO [Node]
- saveState : IO StateHandle
- loadState : StateHandle -> IO ()
- appendToNet : [Node] -> IO ()
- reduceUntilComplete : IO ReductionStats

For now, net construction & read-back can be performed in Haskell. saveState and loadState can be used to copy a in-memory net snapshot - this will allow e.g. first calling createNet with a function, saving the state, appending arguments to the net, reducing, reading the net, and restoring the state prior to calling the function again. That way we should be able to call the same function multiple times reasonably efficiently. Probably a more complex API will be required in the future.

Resources for optimisation (outside of the LLVM library itself):

- [Grin](https://github.com/grin-compiler/grin) has a number of simple optimisation passes. Some may be suitable specifically for a model of a lazy functional graph evaluator but I expect many are helpful here.
- [MLton](http://mlton.org/CompilerOverview) is a whole-program-optimising ML compiler.
- [Slide deck on functional language compilation](https://xavierleroy.org/talks/compilation-agay.pdf) from INRIA.
- [Intel Haskell research compiler](https://github.com/IntelLabs/flrc), which has some optimisation passes.

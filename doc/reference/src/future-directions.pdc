## Zero-knowledge typing

1. Inline ZK verifiers + proofs as custom node types.
1. Type data as "private" or "public" at the monadic level, compiler-enforced.

## Deployment tooling layer

1. Ledgers as first-class objects in declarative deployment scripts.
1. Declarative-stateful deployment system Terraform-style.
1. Blockchains accessible in REPL.

## Interchain abstraction

1. Can run cross-chain over IBC
1. Targets multiple backends (Ethereum, Tezos, Cosmos) initially
1. Avoid lock-in, separate choice of application and choice of consensus

## Visual spatiotemporal dataflow representation

1. Some (closest?) inspiration: Luna [@luna-lang]
1. Could map depths of elementary linear logic terms to spacial depth in an execution visualisation
1. Goal: isomorphism between textual (AST) and graphical (dataflow graph) representations. Getting the isomorphism right so that they can be switched between for real projects seems like the hard part.

## Future optimisation strategies

Juvix does not yet implement these, but the compiler architecture has kept their possibility in mind.

### Stochastic superoptimisation

- Utilise sparse sampling (probably Markov-chain Monte Carlo) to search the configuration space of semantically equivalent programs & select the fastest.
- Probably useful at the level of choosing machine implementations of particular rewrite rules.
- See Stochastic Superoptimisation [@stochastic-superoptimization].
- Will need a lot of clever tricks to avoid getting stuck in local minima (that paper details several).
- See also STOKE [@stoke].

### "Superoptimal" reduction strategies

- Specifically those with the possibility of asymptotically-better performance than Levy's optimal reduction.
- As far as I can tell, the only candidates here are forms of memoisation which attempt to detect syntactically identical structures during the reduction process which can then be linked and evaluated only once.
- [Hash consing](https://en.wikipedia.org/wiki/Hash_consing) may have the most prior research.
- Concerns about space-time trade-offs (already a concern with optimal reduction), likely low-payoff.

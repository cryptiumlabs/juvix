# Juvix

## Overview

Juvix synthesizes a high-level frontend syntax, dependent-linearly-typed core language, and low-level parallelisable
optimally-reducing execution model into a single unified stack for writing formally verifiable, efficiently executable
smart contracts which can be deployed to a variety of distributed ledgers.

Juvix's compiler architecture is purpose-built from the ground up for the particular requirements and economic trade-offs
of the smart contract use case — it prioritises behavioural verifiability, semantic precision, and output code efficiency over compilation speed,
syntactical familiarity, and backwards compatibility with existing blockchain virtual machines.

For details, see [the language reference](./doc/reference/language-reference.pdf).

## Caveats

This is pre-alpha software released for experimentation & research purposes only.

Do not expect API stability. Expect bugs. Expect divergence from canonical protocol implementations.

No warranty is provided or implied.

Juvix is presently executed by a resource-tracing interpreter.

Backends for the EVM, WASM, Michelson, and LLVM are planned but not yet implemented.

## Contributing

See [CONTRIBUTING.md](./doc/CONTRIBUTING.md).

## Installation

Install with ([Stack](https://haskellstack.org) and [z3](https://github.com/Z3Prover/z3) required):

```bash
make
```

For full optimizations (but slower compile times):

```bash
make build-opt
```

To open a REPL with the library scoped:

```bash
make repl-lib
```

To open a REPL with the executable scoped:

```bash
make repl-exe
```

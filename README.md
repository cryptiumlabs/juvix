# Juvix

## Overview

Juvix synthesizes a high-level frontend syntax, dependent-linearly-typed core language, and low-level parallelizable optimally-reducing execution model into a single unified stack for writing formally verifiable, efficiently executable smart contracts which can be deployed to a variety of distributed ledgers.

For details, see [the language reference](./doc/reference/language-reference.pdf).

## Caveats

This is pre-alpha software released for experimentation only.

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

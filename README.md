# Juvix

![Tardigrade](https://upload.wikimedia.org/wikipedia/commons/thumb/c/cd/Water_bear.jpg/279px-Water_bear.jpg)
<br /><sub><sup>([Aditya via Wikimedia Commons, CC-BY-SA 3.0](https://commons.wikimedia.org/wiki/File:Water_bear.jpg))</sup></sub>

![GitHub](https://img.shields.io/github/license/cryptiumlabs/juvix)
![Build status](https://img.shields.io/circleci/build/github/cryptiumlabs/juvix?token=abc123def456)
![GitHub issues](https://img.shields.io/github/issues/cryptiumlabs/juvix)

## Overview

Juvix synthesizes a high-level frontend syntax, dependent-linearly-typed core language, and low-level parallelisable
optimally-reducing execution model into a single unified stack for writing formally verifiable, efficiently executable
smart contracts which can be deployed to a variety of distributed ledgers.

Juvix's compiler architecture is purpose-built from the ground up for the particular requirements and economic trade-offs
of the smart contract use case — it prioritises behavioural verifiability, semantic precision, and output code efficiency over compilation speed,
syntactical familiarity, and backwards compatibility with existing blockchain virtual machines.

> Please note: the frontend language is not yet implemented as we are still working out some details of the type theory & compiler transformations.
  Juvix may end up supporting an existing frontend language (or more than one).

For details, see [the language reference](./doc/reference/language-reference.pdf).

## Caveats

This is pre-alpha software released for experimentation & research purposes only.

Do not expect API stability. Expect bugs. Expect divergence from canonical protocol implementations.

Formal verification of various properties of the Juvix language & compiler in Agda is [in progress](experimental/qtt-agda) but not yet complete.

No warranty is provided or implied.

Juvix is presently executed by a resource-tracing interpreter.

Backends for the EVM, WASM, Michelson, and LLVM are planned but not yet implemented.

## Contributing

See [CONTRIBUTING.md](./doc/CONTRIBUTING.md).

## Installation

Dependencies:

- ([Stack](https://haskellstack.org)
- [z3](https://github.com/Z3Prover/z3)

Install with:

```bash
make
```

For full optimizations (but slower compile times):

```bash
make build-opt
```

## Usage

Juvix is not yet production-ready; however, you can play around with some functionality in an interactive REPL:

```bash
juvix interactive
```

## Development

[Ormolu](https://github.com/cryptiumlabs/ormolu) required for source formatting.

To open a REPL with the library scoped:

```bash
make repl-lib
```

To open a REPL with the executable scoped:

```bash
make repl-exe
```

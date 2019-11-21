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

### Requirements

The following are required:

- [Stack](https://haskellstack.org)
- [z3](https://github.com/Z3Prover/z3)
- [libff](https://github.com/scipr-lab/libff)
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1)
- [Openssl Libssl API](https://wiki.openssl.org/index.php/Libssl_API)
- [LLVM9](https://llvm.org/)

#### Instructions for Linux

- **Stack**
  - For Ubuntu/Debian : `apt install stack`
  - For Arch Linux    : `pacman -S stack`
- **Z3**
  - `make build-z3` while in the `juvix` directory
- **libsecp256k1**
  - For Ubuntu/Debian : `apt install libsecp256k1-dev`
  - For Arch Linux : `pacman -S libsecp256k1`
- **Openssl Libssl API**
  - For Ubuntu/Debian : `apt install libssl-dev`
  - For Arch Linux : `pacman -S openssl`
- **LLVM9**
  - For Arch Linux : `pacman -S llvm`

### Building

Build Juvix and install the binary to the local path with:

```bash
make
```

### Building with optimisations

For full optimisations (but slower compile times):

```bash
make build-opt
```

## Usage

Juvix is not yet production-ready. You can play around with some functionality in an interactive REPL:

```bash
juvix interactive
```

## Development

[Ormolu](https://github.com/cryptiumlabs/ormolu) required for source formatting.

[Quicklisp](https://www.quicklisp.org/beta/) and [sbcl](http://www.sbcl.org/) required for the automatic generation of documentation in [doc/Code](https://github.com/cryptiumlabs/juvix/tree/develop/doc/Code). 

To open a REPL with the library scoped:

```bash
make repl-lib
```

To open a REPL with the executable scoped:

```bash
make repl-exe
```

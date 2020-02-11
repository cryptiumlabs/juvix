# Juvix

![Tardigrade](https://upload.wikimedia.org/wikipedia/commons/thumb/c/cd/Water_bear.jpg/279px-Water_bear.jpg)
<br /><sub><sup>([Aditya via Wikimedia Commons, CC-BY-SA 3.0](https://commons.wikimedia.org/wiki/File:Water_bear.jpg))</sup></sub>

![GitHub](https://img.shields.io/github/license/cryptiumlabs/juvix)
![Build status](https://img.shields.io/circleci/build/github/cryptiumlabs/juvix?token=abc123def456)
![GitHub issues](https://img.shields.io/github/issues/cryptiumlabs/juvix)

## Overview

Juvix synthesizes a high-level frontend syntax, dependent-linearly-typed core language, whole-program optimisation system,
and backend-swappable execution model into a single unified stack for writing formally verifiable, efficiently executable
smart contracts which can be deployed to a variety of distributed ledgers.

Juvix is designed to address the problems that we have experienced while trying to write & deploy decentralised applications and that we observe in the ecosystem at large:
the difficulty of effective verification, the ceiling of compositional complexity, the illegibility of execution costs, and the lock-in to particular backends.
In order to do so, Juvix draws upon and aims to productionise a deep reservoir of prior academic research in programming language design & type theory which we believe has a high degree of applicability to these problems.

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
  - For macOS : `brew install haskell-stack`
- **Z3**
  - `make build-z3` while in the `juvix` directory
- **libsecp256k1**
  - For Ubuntu/Debian : `apt install libsecp256k1-dev`
  - For Arch Linux : `pacman -S libsecp256k1`
  - For macOS : `brew tap cuber/homebrew-libsecp256k1 && brew install libsecp256k1`
- **Openssl Libssl API**
  - For Ubuntu/Debian : `apt install libssl-dev`
  - For Arch Linux : `pacman -S openssl`
  - For macOS : `brew install openssl`
- **LLVM9**
  - For Arch Linux : `pacman -S llvm`
  - For macOS : it requires the latest xcode, then `brew install
    llvm-hs/llvm/llvm-9`,  if it crashes due to “unknown xcode version”, run
    `sudo xcode-select -r`. For newly installed xcode, you will need to accept Apple’s terms and conditions.

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

[Roswell](https://github.com/roswell/roswell) is required for automatic generation of documentation in [doc/Code](https://github.com/cryptiumlabs/juvix/tree/develop/doc/Code).

Once Roswell is installed one only needs to add `~/.roswell/bin` to their bash path along with running `ros install cryptiumlabs/org-generation`.

To open a REPL with the library scoped:

```bash
make repl-lib
```

To open a REPL with the executable scoped:

```bash
make repl-exe
```

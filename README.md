# Juvix

## Overview

Juvix is an experimental research language for writing efficient, safe, and composable smart contracts.

For details, see [the language reference](./doc/reference/language-reference.pdf).

## Caveats

Do not expect API stability. Expect bugs. Expect divergence from canonical protocol implementations.

No warranty is provided or implied.

Backends for the EVM, WASM, Michelson, and LLVM are planned but not yet implemented.

## Contributing

See [CONTRIBUTING.md](./doc/CONTRIBUTING.md).

## Installation

Install with ([Stack](https://haskellstack.org) required):

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

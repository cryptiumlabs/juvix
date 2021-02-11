---
title: Getting Started
position: 1
---

# Getting Started

## Installing Juvix

Please see [the README](https://github.com/metastatedev/juvix#installation).

## Writing your first smart contract

Copy the following to a file named `identity.ju`:

```juvix
open Prelude
open Prelude.Michelson

sig cons-pair : list operation -> int -> pair (list operation) int
let cons-pair = %Michelson.pair

sig nil : list operation
let nil = %Michelson.nil

sig car : pair int int -> int
let car = %Michelson.car

sig main : pair int int -> pair (list operation) int
let main = \params ->
  cons-pair nil (car params)
```

Then run:

```bash
juvix compile identity.ju identity.tz
```

Open `identity.tz` to inspect your compiled contract; it should look like:

```bash
parameter int;
storage int;
code { { DIG 0;
         DUP;
         DUG 1;
         CAR;
         NIL operation;
         PAIR;
         DIP { DROP } } };
```

## Deploying the contract to Tezos

The `.tz` Michelson output of Juvix can be deployed using standard procedures outlined [in the Tezos documentation](https://tezos.gitlab.io/alpha/cli-commands.html?highlight=originate). Direct integration with the Juvix toolchain is coming soon.

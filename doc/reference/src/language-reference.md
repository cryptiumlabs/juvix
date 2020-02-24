---
title: Juvix — Language Reference
lang: en-GB
titlepage: true
titlepage-color: "06386e"
titlepage-text-color: "FFFFFF"
titlepage-rule-color: "FFFFFF"
titlepage-rule-height: 1
toc-own-page: true
mainfont: DejaVuSerifCondensed
fontsize: 8pt
author: \small Christopher Goes, Marty Stumpf, Jeremy Ornelas, Andy Morris, Asher Manning
date: \today \ - \textit{Prerelease}
urlcolor: cyan
abstract: Juvix synthesises a high-level frontend syntax, dependent-linearly-typed core language, and low-level parallelisable
          optimally-reducing execution model into a single unified stack for writing formally verifiable, efficiently executable
          smart contracts which can be deployed to a variety of distributed ledgers.
          \par
          Juvix's compiler architecture is purpose-built from the ground up for the particular requirements and economic trade-offs
          of the smart contract use case — it prioritises behavioural verifiability, semantic precision, and output code efficiency over compilation speed,
          syntactical familiarity, and backwards compatibility with existing blockchain virtual machines.
          \par
          Machine-assisted proof search, declarative deployment tooling, type & usage inference, and alternative spatiotemporal dataflow representations facilitate
          integration of low-developer-overhead property verification into the development process.
          An interchain abstraction layer representing ledgers as first-class objects enables seamless cross-chain programming and type-safe runtime reconfiguration.
          \par
          This document is designed to be a first-principles explanation of Juvix. No familiarity with the theoretical background is assumed.
          Readers previously acquainted with the lambda calculus, sequent calculus, simply-typed lambda calculus, the calculus of constructions,
          linear logic, interaction nets, elementary affine logic, and Lamping's optimal reduction algorithm may skip the associated subsections in chapter five.
---

\pagebreak


# Motivation

!include src/motivation.md

# Typographical conventions

!include src/typographical-conventions.md

# Prior work

!include src/prior-work.md

# Desiderata

!include src/desiderata.md

# Ingredients

!include src/ingredients.md

# Theoretical background

!include src/theoretical-background.md

## Lambda calculus

!include src/background/lambda-calculus.md

## Sequent calculus

!include src/background/sequent-calculus.md

## Simply-typed lambda calculus

!include src/background/simply-typed-lambda-calculus.md

## Calculus of constructions

!include src/background/calculus-of-constructions.md

## Linear logic

!include src/background/linear-logic.md

## Interaction nets

!include src/background/interaction-nets.md

## Elementary affine logic

!include src/background/elementary-affine-logic.md

## Optimal reduction

!include src/background/optimal-reduction.md

\pagebreak

# Architectural overview

!include src/architecture.md

# Frontend language

!include src/frontend-language.md

# Core language

!include src/core-language.md

# Core optimisation

!include src/core-optimisation.md

# Erased core language

!include src/erased-core-language.md

# Elementary affine core

!include src/elementary-affine-core.md

# Low-level execution model

!include src/low-level-execution-model.md

# Cost accounting

!include src/cost-accounting.md

# Backends

!include src/backends.md

# Distributed ledger integration

!include src/distributed-ledger-integration.md

# Future directions

!include src/future-directions.md

# Examples

!include src/examples.md

\pagebreak

# References

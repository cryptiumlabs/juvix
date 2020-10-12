*What problems within the decentralised application ecosystem at large could be addressed at the language level?*

*What form should a better contract language take & what requirements must it fulfil?*

## Start with a pure functional basis

Many of the observed mistakes & bugs in smart contracts are not specific to the blockchain use-case at all —
they are just common errors made in imperative languages by programmers everywhere: unintentional side-effects,
type mismatches, and failure to handle exceptions or unexpected states.

Pure functional languages, with strongly-typed terms, immutable data, first-class functions, and referential transparency,
are the right basis for a contract language. A strong typesystem enables the typechecker to catch many errors
at compile time which would have otherwise resulted in misbehaviour at runtime. Purity, immutable data & referential transparency
simplify the mental context a programmer must have, since they no longer need to reason about side effects.
First-class functions allow the modularisation of complex logic into individually digestible, composable parts.

## Enable an ecosystem of verification

"Formal verification" has been rendered a bit of a buzzword, but the actual need remains mostly unaddressed:
a mathematically sound, legible way for users of decentralised applications to reason about what their interactions
with an application will or will not do: whether a token really behaves like a currency, whether an exchange contract
could accidentally steal their funds, or whether a derivative will always be sufficiently collataralised.

Exposing formal verification tooling to contract developers is necessary, but not sufficient — it must be possible
for third-party software providers, such as wallet authors & application frontend developers, to embed
proof-checking into their application and translate success or failure of verifying particular behaviours
into legible UI elements — a green checkmark if the token contract is compliant, a red warning otherwise.

Contract property verification must be progressive — proof-construction UX improvements notwithstanding,
it will always make little sense to invest time to verify beta applications or experiments. Developers
must be able to write contracts, modify them, and progressively verify properties over time, often
after the original code has been written.

The state machines of blockchains must eventually understand this verification system, and use that
understanding to allow contracts to reason about how other contracts with which they interact will or
will not behave, just as users can, and elect only to interact with contracts which can prove that they
fulfil certain properties. This will require seamless integration of the verification system into the language itself.

## Eliminate the compositional complexity ceiling

Distributed applications are most compelling, and most competitive with their centralised brethren, when
they can benefit from the network effects of permissionless interoperation without the bottlenecks of trusted
organisational relationships: anyone in the world can deploy a new contract which interacts with yours, perhaps
to provide a new feature or application which you hadn't even conceived of. Alas, this potential network effect
is severely curtailed by the sheer difficulty of reasoning about complex, multi-layered compositions of contracts.
Because present languages cannot provide checkable guarantees about the behaviour of their own terms, each line
of code in a contract must consider what every other line of code might do — to first order, this renders the 
mental cost of reasoning about the correctness of a contract quadratic in the size of the codebase. Developers
must have the ability to articulate predicates which tightly constrain the behaviour of its own terms in the language
itself, and thus eliminate entirely the necessity of tracking the behaviour of some particular dependency, since
it has been constrained to exactly what is required and no more.

## Provide predictable resource consumption

Resources consumed while executing contracts on replicated ledgers must be paid for. Illegibility and unpredictability
of such payments are at present a major UX friction point for contract developers and application users alike. The
resource cost of calling a contract must be exactly calculable or at least closely bounded prior to executing the call,
so that developers can compare the costs of different code-paths at compile time and users can ensure that they have provided
sufficient payment for complete execution. Should the ledger elect to integrate the typechecker into the state machine
logic directly, it should be possible to perform these resource cost verifications instead of metering gas at runtime.

## Abstract away from particular backends

Blockchains come & go, and the virtual machines or execution environments which they provide improve & change over time.
Developers of decentralised applications should be able to abstract over these implementation particulars and write contracts
at a sufficiently high-level to capture instead the essential semantics of their intentions, then redeploy these contracts
to multiple ledgers and updated execution environments without rewriting them each time. Optimisations for particular
execution environments or ledgers should be logically centralised, handled once by updated compiler pipelines instead of
individually by each application developer. Successful abstraction will empower not only contract developers but
also protocol engineers: virtual machines can be upgraded more quickly if contracts can easily be recompiled to target
new ones, and exit costs of particular ledgers will be reduced if contract code can be re-targeted to other ones.

## Retain efficient execution

Execution of contract code is expensive, whether replicated across validation nodes of a distributed ledger,
or simulated within the restricted setting of an interactive prover. Formal verification, resource consumption
proofs, and backend abstraction must not come at the cost of runtime performance penalties. The unique properties
of the smart-contract use case — small code sizes and infrequent deployment — should be leveraged to enable
optimisations which might not be feasible in a more general-purpose language.

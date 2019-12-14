Ideally a developer-facing language would operate at a higher level, automatically determining the necessary registers, length of execution trace, transition function & transition constraints. I think this should be possible to do automatically pretty efficiently - possibly in some cases custom constraints could be defined & proved to be equivalent to correct execution of the transition function (e.g., for a layer l_n in a trace, l_n = f(l_n-1) <=> c l_n l_n-1 where c is the constraint relation).

I am not sure if an interaction net transition function can be efficiently encoded in a STARK. I think the transition function itself is less likely to be a problem than the memory accesses (which will require a Merkle tree & proofs for all reads, probably). The concrete numbers here (especially prover time & proof size) may be prohibitively high. Apparently 1-layer recursion in STARKs is possible; I do not know the details. DEEP-FRI should also help.

At minimum, even with "function-to-constraints" compilation, a custom library will be necessary to import STARK-friendly hash functions, signature schemes, etc.

- Construct registers to hold all values
- Use public / private type distinctions for register separation
- ADTs must be completely erased by runtime
- Require primitive type (or tuple, or encodable/decodable ADT) input, output
- Specific primitive types supported by STARKs (field-dependent)
- Unroll loops completely, must have finite length

Questions:
- Can QTT provide sufficient precision to allow all register allocation to happen statically?
- If we inline everything, can higher-order functions be erased (want to avoid interaction net evaluation, since that requires dynamic memory allocation)?
- Transforms to reuse registers over time ~ may be prior literature or forms, e.g. SSA
- Figure out rough numbers for size, proof time scaling vs. register count

Automatic split & fill a la Agda `auto`.

See [paper](https://wenkokke.github.io/pubs/mpc2015.pdf).

Possible alterations/additions:
- Alterations to take into account usage constraints (when known)
- Try case-splitting where applicable (& continue searching)
- Attempt to prove the negation
- Include constants in scope, use A-star search instead of DFS, with a heuristic based on usage correlation across modules

In the future, maybe interface with an automated prover.

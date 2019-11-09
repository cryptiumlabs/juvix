{-# LANGUAGE CPP #-}
{- This is used to only build with full optimisations when building in release mode, since building with full optimisations is apparently very slow. -}

module Juvix.Backends.Michelson.Optimisation where

import Juvix.Library
import Michelson.Typed

{-  This is a simple optimization strategy which replaces sequences of Michelson instructions with equivalent sequences of fewer instructions.
    Runs optimization passes until no further optimizations are found, up to a maximum number of passes.
    At the moment nontrivial programs are unlikely to compile to the smallest equivalent Michelson instruction sequence, but little time has been spent on optimization so far - a high degree should be possible; the Haskell typesystem provides very strong guarantees.
    A more interesting / potentially more effective strategy might be to search the space of equivalent Michelson programs, which at small program sizes using bounded heuristic search should be computationally feasible
      - then choose the one with the fewest instructions (or based on some other gas-estimation preference function).
    This optimization function is typed in the Instr GADT, so it cannot produce invalid output Michelson. However, the typesystem does not enforce computation correctness; that would require dependent types. -}

optimise ∷ ∀ a b m. (Monad m) ⇒ Instr a b → m (Instr a b)
optimise expr = do
  let tellReturn ret = return ret -- tell [optimised (SomeExpr expr) (SomeExpr ret)] >> return ret
      inner e = do
        one ← optimise' e
        two ← optimise' one
        if one == two then return two else inner two
  inner expr

optimise' ∷ ∀ a b m. (Monad m) ⇒ Instr a b → m (Instr a b)
optimise' expr =
  case expr of
    (IF x y) → optimise' x >>= \x → optimise' y >>= \y → return (IF x y)
    (IF_LEFT x y) → optimise' x >>= \x → optimise' y >>= \y → return (IF_LEFT x y)
    (IF_CONS x y) → optimise' x >>= \x → optimise' y >>= \y → return (IF_CONS x y)
    (IF_NONE x y) → optimise' x >>= \x → optimise' y >>= \y → return (IF_NONE x y)
    (DIP e) → DIP |<< optimise' e
    {- We can assume Seq will be left-forced. -}
#if defined(OPTIMIZE)
    (Seq (Seq DUP (DIP e)) DROP) → optimise' e
    (Seq (DIP e) DROP) → optimise' (Seq DROP e)
    (Seq (Seq (Seq e DUP) SWAP) DROP) → optimise' e
    (Seq (Seq DUP SWAP) DROP) → return Nop
    (Seq DUP (DIP DROP)) → return Nop
    (Seq (Seq SWAP DUP) (DIP SWAP)) → return (Seq (DIP DUP) SWAP)
    (Seq DUP SWAP) → return DUP
    (Seq DUP DROP) → return Nop
    (Seq SWAP SWAP) → return Nop
    (Seq FAILWITH e) → return FAILWITH
#endif
    (Seq e Nop) → optimise' e
    (Seq Nop e) → optimise' e
    (Seq x y) → optimise' x >>= \x → optimise' y >>= \y → return (Seq x y)
    expr → return expr

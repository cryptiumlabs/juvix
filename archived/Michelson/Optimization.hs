{-# LANGUAGE CPP #-}

module Juvix.Backends.Michelson.Optimization where

import           Control.Monad.Writer
import           Protolude                                  hiding (Const (..))

import           Juvix.Backends.Michelson.Compilation.Types
import           Juvix.Backends.Michelson.Typed
import           Juvix.Utility

{-  Exported for testing convenience.   -}

optimizeNoLogs :: (Dynamical a, Dynamical b) ⇒ Expr (Stack a) (Stack b) → Expr (Stack a) (Stack b)
optimizeNoLogs = fst . runWriter . optimize

{-  This is a simple optimization strategy which replaces sequences of Michelson instructions with equivalent sequences of fewer instructions.
    Runs optimization passes until no further optimizations are found, up to a maximum number of passes.
    At the moment nontrivial programs are unlikely to compile to the smallest equivalent Michelson instruction sequence, but little time has been spent on optimization so far - a high degree should be possible; the Haskell typesystem provides very strong guarantees.
    A more interesting / potentially more effective strategy might be to search the space of equivalent Michelson programs, which at small program sizes using bounded heuristic search should be computationally feasible
      - then choose the one with the fewest instructions (or based on some other preference function, depending on how Tezos ends up pricing contract execution).
    This optimization function is typed in the Expr GADT, so it cannot produce invalid output Michelson. However, the typesystem does not enforce computation correctness; that would require dependent types. -}

optimize :: (Dynamical a, Dynamical b, MonadWriter [CompilationLog] m) ⇒ Expr (Stack a) (Stack b) → m (Expr (Stack a) (Stack b))
optimize expr = do
  let tellReturn ret = tell [Optimized (SomeExpr expr) (SomeExpr ret)] >> return ret
      inner e = do
        one ← optimize' e
        two ← optimize' one
        if one == two then tellReturn two else inner two
  inner expr

optimize' :: (Dynamical a, Dynamical b, MonadWriter [CompilationLog] m) ⇒ Expr (Stack a) (Stack b) → m (Expr (Stack a) (Stack b))
optimize' expr =
  case expr of

    (If x y)        -> optimize' x >>= \x -> optimize' y >>= \y -> return (If x y)
    (IfLeft x y)    -> optimize' x >>= \x -> optimize' y >>= \y -> return (IfLeft x y)
    (IfCons x y)    -> optimize' x >>= \x -> optimize' y >>= \y -> return (IfCons x y)
    (IfNone x y)    -> optimize' x >>= \x -> optimize' y >>= \y -> return (IfNone x y)
    (Dip e)         -> Dip |<< optimize' e

{- We can assume Seq will be left-forced. -}
#if defined(OPTIMIZE)
    (Seq (Seq Dup (Dip e)) Drop)        -> optimize' e
    (Seq (Dip e) Drop)                  -> optimize' (Seq Drop e)
    (Seq (Seq (Seq e Dup) Swap) Drop)   -> optimize' e

    -- (Seq (Seq (Seq Dup Cdr) Swap) Car)  -> (Seq (Seq Dup (Dip Cdr)) Car)

    (Seq (Seq Dup Swap) Drop)           -> return Nop
    (Seq Dup (Dip Drop))                -> return Nop

    (Seq (Seq Swap Dup) (Dip Swap))     -> return (Seq (Dip Dup) Swap)

    (Seq Dup Swap)  -> return Dup
    (Seq Dup Drop)  -> return Nop
    (Seq Swap Swap) -> return Nop
    (Seq Fail e)    -> return Fail
#endif

    (Seq e Nop)     -> optimize' e
    (Seq Nop e)     -> optimize' e

    (Seq x y)       -> optimize' x >>= \x -> optimize' y >>= \y -> return (Seq x y)

    expr            -> return expr

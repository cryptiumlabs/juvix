-- | This is a simple optimization strategy which replaces sequences of Michelson instructions with equivalent sequences of fewer instructions.
--    At the moment nontrivial programs are unlikely to compile to the smallest equivalent Michelson instruction sequence,
-- but little time has been spent on optimization so far - a high degree should be possible; the Haskell typesystem provides very strong guarantees.
--    A more interesting / potentially more effective strategy might be to search the space of equivalent Michelson programs,
--  which at small program sizes using bounded heuristic search should be computationally feasible -
--  then choose the one with the fewest instructions (or based on some other gas-estimation preference function).
--    This optimization function is typed in the Instr GADT, so it cannot produce invalid output Michelson.
--  However, the typesystem does not enforce computation correctness; that would require dependent types.
module Juvix.Backends.Michelson.Optimisation where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import Michelson.Typed
import qualified Michelson.Untyped as U

optimise ∷
  ∀ a b m.
  (HasWriter "compilationLog" [CompilationLog] m) ⇒
  Instr a b →
  m (Instr a b)
optimise instr = do
  let inner e = do
        one ← optimise' e
        two ← optimise' one
        if one == two then pure two else inner two
  ret ← inner instr
  tell @"compilationLog" [Optimised (SomeInstr instr) (SomeInstr ret)]
  pure ret

optimise' ∷
  ∀ a b m.
  (Monad m) ⇒
  Instr a b →
  m (Instr a b)
optimise' expr =
  case expr of
    (IF x y) → IF <$> optimise' x <*> optimise' y
    (IF_LEFT x y) → IF_LEFT <$> optimise' x <*> optimise' y
    (IF_CONS x y) → IF_CONS <$> optimise' x <*> optimise' y
    (IF_NONE x y) → IF_NONE <$> optimise' x <*> optimise' y
    (DIP e) → DIP |<< optimise' e
    -- This can't be done in the typed form.
    --(Seq (Seq op (LAMBDA (VLam l))) EXEC) -> pure (Seq op l)
    {- We can assume Seq will be left-forced. -}
    (Seq (Seq DUP (DIP e)) DROP) → optimise' e
    (Seq (DIP e) DROP) → optimise' (Seq DROP e)
    (Seq (Seq (Seq e DUP) SWAP) DROP) → optimise' e
    (Seq (Seq DUP SWAP) DROP) → pure Nop
    (Seq DUP (DIP DROP)) → pure Nop
    (Seq (Seq SWAP DUP) (DIP SWAP)) → pure (Seq (DIP DUP) SWAP)
    (Seq DUP SWAP) → pure DUP
    (Seq DUP DROP) → pure Nop
    (Seq SWAP SWAP) → pure Nop
    (Seq FAILWITH _) → pure FAILWITH
    (Seq e Nop) → optimise' e
    (Seq Nop e) → optimise' e
    (Seq x y) → Seq <$> optimise' x <*> optimise' y
    expr → pure expr

optimiseUntyped ∷
  ∀ m.
  (HasWriter "compilationLog" [CompilationLog] m) ⇒
  Op →
  m Op
optimiseUntyped op = do
  let inner o = do
        let one = optimiseUntypedSingle o
            two = optimiseUntypedSingle one
        if one == two then pure two else inner two
  ret ← inner op
  -- TODO: log, typecheck
  pure ret

optimiseUntypedSeq ∷ [Op] → [Op]
optimiseUntypedSeq ops =
  case ops of
    (U.PrimEx (U.DUP _) : U.PrimEx (U.DIP e) : U.PrimEx U.DROP : rest) → U.SeqEx e : rest
    (op : rest) → optimiseUntypedSingle op : optimiseUntypedSeq rest
    [] → []

optimiseUntypedSingle ∷ Op → Op
optimiseUntypedSingle op =
  case op of
    U.PrimEx prim → U.PrimEx $
      case prim of
        U.IF a b → U.IF (optimiseUntypedSeq a) (optimiseUntypedSeq b)
        instr → instr
    U.SeqEx seq → U.SeqEx (optimiseUntypedSeq seq)
    U.WithSrcEx s op → U.WithSrcEx s (optimiseUntypedSingle op)

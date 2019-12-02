-- |
-- - This is a simple optimization strategy which replaces sequences of
--     Michelson instructions with equivalent sequences of fewer
--     instructions.
--   + At the moment nontrivial programs are unlikely to compile to
--     the smallest equivalent Michelson instruction sequence,
-- - but little time has been spent on optimization so far - a high
--   degree should be possible; the Haskell typesystem provides very
--   strong guarantees.
--   + A more interesting / potentially more effective strategy might
--     be to search the space of equivalent Michelson programs,
--     which at small program sizes using bounded heuristic search
--     should be computationally feasible -
--     then choose the one with the fewest instructions (or based on
--     some other gas-estimation preference function).
module Juvix.Backends.Michelson.Optimisation where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Parameterisation
import Juvix.Library
import Michelson.Untyped hiding (Op)

optimise ∷
  ∀ m.
  (HasWriter "compilationLog" [CompilationLog] m) ⇒
  Op →
  m Op
optimise op = do
  let inner o = do
        let one = optimiseSingle o
            two = optimiseSingle one
        if one == two then pure two else inner two
  ret ← inner op
  tell @"compilationLog" [Optimised op ret]
  pure ret

optimiseSeq ∷ [Op] → [Op]
optimiseSeq ops =
  case ops of
    {- Simple stack manipulations. -}
    (PrimEx (DUP _) : PrimEx (DIP e) : PrimEx DROP : rest) → e <> rest
    (PrimEx (DIP e) : PrimEx DROP : rest) → PrimEx DROP : e <> rest
    (PrimEx (DUP _) : PrimEx SWAP : PrimEx DROP : rest) → rest
    (PrimEx (DUP _) : PrimEx (DIP [PrimEx DROP]) : rest) → rest
    (PrimEx SWAP : PrimEx (DUP _) : PrimEx (DIP [PrimEx SWAP]) : rest) → PrimEx (DIP [PrimEx (DUP "")]) : PrimEx SWAP : rest
    (PrimEx (DUP ann) : PrimEx SWAP : rest) → PrimEx (DUP ann) : rest
    (PrimEx SWAP : PrimEx SWAP : rest) → rest
    (PrimEx (DUP _) : PrimEx DROP : rest) → rest
    {- Failures. -}
    (PrimEx FAILWITH : _) → [PrimEx FAILWITH]
    {- Lambda / exec. -}
    (PrimEx (LAMBDA _ _ _ l) : PrimEx (EXEC _) : rest) → l <> rest
    --(Seq (Seq op (LAMBDA (VLam l))) EXEC) -> pure (Seq op l)
    {- Recursive case. -}
    (op : rest) → optimiseSingle op : optimiseSeq rest
    {- Empty case. -}
    [] → []

optimiseSingle ∷ Op → Op
optimiseSingle op =
  case op of
    PrimEx prim → PrimEx $
      case prim of
        IF a b → IF (optimiseSeq a) (optimiseSeq b)
        IF_LEFT a b → IF_LEFT (optimiseSeq a) (optimiseSeq b)
        IF_CONS a b → IF_CONS (optimiseSeq a) (optimiseSeq b)
        IF_NONE a b → IF_NONE (optimiseSeq a) (optimiseSeq b)
        DIP a → DIP (optimiseSeq a)
        instr → instr
    SeqEx seq → SeqEx (optimiseSeq seq)
    WithSrcEx s op → WithSrcEx s (optimiseSingle op)

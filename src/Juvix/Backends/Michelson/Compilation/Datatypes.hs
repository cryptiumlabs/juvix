-- |
-- Datatypes & pattern matching.
module Juvix.Backends.Michelson.Compilation.Datatypes where

import Juvix.Backends.Michelson.Compilation.Types
import Juvix.Backends.Michelson.Compilation.Util
import Juvix.Library hiding (Type)
import Michelson.Untyped

pack ∷
  ∀ m.
  (HasThrow "compilationError" CompilationError m) ⇒
  Type →
  m ExpandedInstr
pack (Type TUnit _) = pure (PUSH "" (Type TUnit "") ValueUnit)
pack ty = throw @"compilationError" (NotYetImplemented ("pack: " <> show ty))

unpack ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  Type →
  [Maybe Symbol] →
  m ExpandedOp
unpack (Type ty _) binds =
  case ty of
    Tbool → do
      modify @"stack" (drop 1)
      return (SeqEx [])
    TPair _ _ fT sT →
      case binds of
        [Just fst, Just snd] → do
          modify @"stack" (appendDrop [(VarE fst, fT), (VarE snd, sT)])
          pure (SeqEx [PrimEx (DUP ""), PrimEx (CDR "" ""), PrimEx SWAP, PrimEx (CAR "" "")])
        [Just fst, Nothing] → do
          modify @"stack" (appendDrop [(VarE fst, fT)])
          pure (PrimEx (CAR "" ""))
        [Nothing, Just snd] → do
          modify @"stack" (appendDrop [(VarE snd, sT)])
          pure (PrimEx (CDR "" ""))
        [Nothing, Nothing] →
          genReturn (PrimEx DROP)
        _ → throw @"compilationError" (InternalFault "binds do not match type")
    _ → throw @"compilationError" (NotYetImplemented ("unpack: " <> show ty))
unpack _ _ = throw @"compilationError" (InternalFault "invalid unpack type")

unpackDrop ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  [Maybe Symbol] →
  m ExpandedOp
unpackDrop binds = genReturn (foldDrop (fromIntegral (length (filter isJust binds))))

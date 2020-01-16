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
      modify @"stack" (dropS (1 ∷ Int))
      return (SeqEx [])
    TPair _ _ fT sT →
      case binds of
        [Just fst, Just snd] → do
          modify @"stack" (appendDrop (fromList [(VarE fst Nothing, fT), (VarE snd Nothing, sT)]))
          pure (SeqEx [PrimEx (DUP ""), PrimEx (CDR "" ""), PrimEx SWAP, PrimEx (CAR "" "")])
        [Just fst, Nothing] → do
          modify @"stack" (appendDrop (fromList [(VarE fst Nothing, fT)]))
          pure (PrimEx (CAR "" ""))
        [Nothing, Just snd] → do
          modify @"stack" (appendDrop (fromList [(VarE snd Nothing, sT)]))
          pure (PrimEx (CDR "" ""))
        [Nothing, Nothing] →
          genReturn (PrimEx DROP)
        _ → throw @"compilationError" (InternalFault "binds do not match type")
    _ → throw @"compilationError" (NotYetImplemented ("unpack: " <> show ty))

unpackDrop ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m
  ) ⇒
  [Maybe Symbol] →
  m ExpandedOp
unpackDrop binds = genReturn (foldDrop (fromIntegral (length (filter isJust binds))))

genSwitch ∷
  ∀ m.
  ( HasState "stack" Stack m,
    HasThrow "compilationError" CompilationError m,
    HasWriter "compilationLog" [CompilationLog] m
  ) ⇒
  T →
  m (ExpandedOp → ExpandedOp → ExpandedOp)
genSwitch Tbool = pure (\x y → PrimEx (IF [y] [x])) -- TODO: Why flipped?
genSwitch (TOr _ _ _ _) = pure (\x y → PrimEx (IF_LEFT [x] [y]))
genSwitch (TOption _) = pure (\x y → PrimEx (IF_NONE [x] [y]))
genSwitch (TList _) = pure (\x y → PrimEx (IF_CONS [x] [y]))
genSwitch ty = throw @"compilationError" (NotYetImplemented ("genSwitch: " <> show ty))

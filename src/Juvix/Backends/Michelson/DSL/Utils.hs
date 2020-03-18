module Juvix.Backends.Michelson.DSL.Utils where

import Juvix.Library
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import qualified Michelson.Untyped.Instr as Instr
import qualified Juvix.Core.ErasedAnn.Types as Ann
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions


piToList ∷ Ann.Type primTy primVal → [(Usage.T, Ann.Type primTy primVal)]
piToList (Ann.Pi usage aType rest) = (usage, aType) : piToList rest
piToList _ = []

unpackTuple ∷ Instr.ExpandedOp
unpackTuple =
  Instructions.dup
    <> Instructions.car
    <> Instructions.dip [Instructions.cdr]

unpackTupleN ∷ Natural → Instr.ExpandedOp
unpackTupleN 0 = mempty
unpackTupleN n = unpackTuple <> Instructions.dip [unpackTupleN (pred n)]

-- (captures, args) => args ... : captures ... : []
unpackArgsCaptures ∷ Natural → Natural → Instr.ExpandedOp
unpackArgsCaptures numArgs numCaptures =
  Instructions.dup
    <> Instructions.dip [Instructions.car, unpackTupleN (numCaptures - 1)]
    <> Instructions.cdr
    <> unpackTupleN (numArgs - 1)

pairN ∷ Int → Instr.ExpandedOp
pairN count = fold (replicate count Instructions.pair)

closureType ∷ [(Symbol, Untyped.T)] → Untyped.T
closureType = foldr (Untyped.pair . snd) Untyped.unit

-- | 'lamType' takes Args+Closures and ExtraArgs, along with their return type
-- and constructs a lambda type
lamType ∷ [(Symbol, Untyped.T)] → [(Symbol, Untyped.T)] → Untyped.T → Untyped.T
lamType argsPlusClosures =
  Untyped.lambda
    . Untyped.pair (closureType argsPlusClosures)
    . closureType

module Juvix.Backends.Michelson.DSL.Utils where

import qualified Juvix.Backends.Michelson.DSL.Instructions as Instructions
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Michelson.Untyped.Instr as Instr


unpackTuple :: Instr.ExpandedOp
unpackTuple =
  Instructions.dup
    <> Instructions.car
    <> Instructions.dip [Instructions.cdr]

unpackTupleN :: Natural -> Instr.ExpandedOp
unpackTupleN 0 = mempty
unpackTupleN n = unpackTuple <> Instructions.dip [unpackTupleN (pred n)]

-- (captures, args) => args ... : captures ... : []
unpackArgsCaptures :: Natural -> Natural -> Instr.ExpandedOp
unpackArgsCaptures numArgs numCaptures =
  Instructions.dup
    <> Instructions.dip [Instructions.car, unpackTupleN (pred numCaptures)]
    <> Instructions.cdr
    <> unpackTupleN (pred numArgs)

pairN :: Int -> Instr.ExpandedOp
pairN count = fold (replicate count Instructions.pair)

closureType :: [(NameSymbol.T, Untyped.T)] -> Untyped.T
closureType = foldr (Untyped.pair . snd) Untyped.unit

-- | 'lamType' takes Args+Closures and ExtraArgs, along with their return type
-- and constructs a lambda type
lamType :: [(NameSymbol.T, Untyped.T)] -> [(NameSymbol.T, Untyped.T)] -> Untyped.T -> Untyped.T
lamType argsPlusClosures =
  Untyped.lambda
    . Untyped.pair (closureType argsPlusClosures)
    . closureType

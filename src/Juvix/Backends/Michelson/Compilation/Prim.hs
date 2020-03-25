-- |
-- - Compilation of primitive terms to Michelson instruction sequences.
module Juvix.Backends.Michelson.Compilation.Prim where

import qualified Juvix.Backends.Michelson.Compilation.Environment as Env
import qualified Juvix.Backends.Michelson.Compilation.Type as Type
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import Juvix.Library
import qualified Michelson.Untyped as M

promoteInStack :: HasState "stack" VStack.T f => Int -> f [M.ExpandedOp]
promoteInStack n = fst . VStack.promote n <$> get@"stack"

primToInstr ::
  forall m.
  ( HasState "stack" VStack.T m,
    HasThrow "compilationError" Env.CompilationError m,
    HasWriter "compilationLog" [Env.CompilationLog] m
  ) =>
  Env.PrimVal ->
  ErasedAnn.Type Env.PrimTy Env.PrimVal ->
  m Env.Op
primToInstr prim ty =
  -- Inline all functions, assumed they will have been correctly dealt with previously.
  -- Think about whether the order will be correct here - it matters.
  -- TODO ∷ do analysis for when we can move the constants into being constant
  --        operations instead of just promoting them
  -- If arguments are both constants, drop both from vstack, PUSH the addition (e.g.)
  -- If one is real, one is not, push the constant to the stack, add an ADD instruction.
  case prim of
    -- :: \x -> y ~ s => (f, s)
    Env.PrimFst -> do
      let ErasedAnn.Pi _ (ErasedAnn.PrimTy (Env.PrimTy (M.Type (M.TPair _ _ xT _) _))) _ = ty
      promoted <- promoteInStack 1
      modify@"stack" (VStack.cons (VStack.Val VStack.FuncResultE, xT) . VStack.drop 1)
      pure (M.SeqEx (promoted <> [M.PrimEx (M.CAR "" "")]))
    Env.PrimSnd -> do
      let ErasedAnn.Pi _ (ErasedAnn.PrimTy (Env.PrimTy (M.Type (M.TPair _ _ _ yT) _))) _ = ty
      promoted <- promoteInStack 1
      modify@"stack" (VStack.cons (VStack.Val VStack.FuncResultE, yT) . VStack.drop 1)
      pure (M.SeqEx (promoted <> [M.PrimEx (M.CDR "" ""), M.PrimEx (M.CAR "" "")]))
    Env.PrimPair -> do
      let ErasedAnn.Pi _ firstArgTy (ErasedAnn.Pi _ secondArgTy _) = ty
      xT <- Type.typeToType firstArgTy
      yT <- Type.typeToType secondArgTy
      let pairTy = M.Type (M.TPair "" "" xT yT) ""
      promoted <- promoteInStack 2
      modify@"stack" (VStack.cons (VStack.Val VStack.FuncResultE, pairTy) . VStack.drop 2)
      pure (M.SeqEx (promoted <> [M.PrimEx (M.PAIR "" "" "" "")]))
    -- :: a ~ s => (a, s)
    Env.PrimConst const -> do
      case const of
        M.ValueNil -> do
          let ErasedAnn.PrimTy (Env.PrimTy t@(M.Type (M.TList elemTy) _)) = ty
          modify@"stack" (VStack.cons (VStack.Val VStack.FuncResultE, t))
          pure (M.PrimEx (M.NIL "" "" elemTy))
        _ -> do
          let ErasedAnn.PrimTy (Env.PrimTy t) = ty
          modify@"stack" (VStack.cons (VStack.Val VStack.FuncResultE, t))
          pure (M.PrimEx (M.PUSH "" t const))

{-# LANGUAGE OverloadedStrings #-}

module Juvix.Backends.LLVM.Compilation
  ( compileProgram,
  )
where

import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import Juvix.Library.Feedback
import qualified LLVM.AST as LLVM (Module, Operand)
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder as LLVM
import qualified LLVM.Pretty as LLVM
import qualified Prelude as P

compileProgram ::
  Monad m =>
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  FeedbackT [] P.String m Text
compileProgram t@(ErasedAnn.Ann usage ty _) = do
  let llvmmod :: LLVM.Module
      llvmmod = LLVM.buildModule "juvix-module" $ do
        LLVM.function "main" [] (typeToLLVM ty) $ \[] -> do
          out <- compileTerm t
          LLVM.ret out
  return $ toStrict $ LLVM.ppllvm llvmmod

compileTerm ::
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
compileTerm (ErasedAnn.Ann usage ty t) = case t of
  ErasedAnn.Prim t' -> mkPrim t
  ErasedAnn.AppM f xs -> mkApp (ErasedAnn.term f) ty xs

mkPrim ::
  ErasedAnn.Term PrimTy RawPrimVal ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkPrim (ErasedAnn.Prim prim) = case prim of
  LitInt i -> return $ LLVM.int8 i -- TODO deal with the type correctly.

mkApp ::
  ErasedAnn.Term PrimTy RawPrimVal ->
  ErasedAnn.Type PrimTy ->
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkApp f (ErasedAnn.PrimTy ty) xs = case f of
  ErasedAnn.Prim prim -> applyPrim prim ty xs

applyPrim ::
  RawPrimVal ->
  PrimTy ->
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
applyPrim f ty xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm (xs P.!! 0)
        y <- compileTerm (xs P.!! 1)
        LLVM.add x y

typeToLLVM :: ErasedAnn.Type PrimTy -> LLVM.Type
typeToLLVM (ErasedAnn.PrimTy (PrimTy ty)) = ty

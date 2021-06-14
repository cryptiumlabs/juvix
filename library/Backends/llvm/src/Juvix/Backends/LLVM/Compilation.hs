{-# LANGUAGE OverloadedStrings #-}

module Juvix.Backends.LLVM.Compilation
  ( compileProgram,
  )
where

import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.ErasedAnn as Core
import Juvix.Library
import Juvix.Library.Feedback
import qualified LLVM.AST as LLVM (Module, Operand)
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder as LLVM
import qualified LLVM.Pretty as LLVM
import qualified Prelude as P

compileProgram ::
  Monad m =>
  Core.AnnTerm PrimTy RawPrimVal ->
  FeedbackT [] P.String m Text
compileProgram t@(Core.Ann usage ty _) = do
  let llvmmod :: LLVM.Module
      llvmmod = LLVM.buildModule "juvix-module" $ do
        LLVM.function "main" [] (typeToLLVM ty) $ \[] -> do
          out <- compileTerm t
          LLVM.ret out
  return $ toStrict $ LLVM.ppllvm llvmmod

compileTerm ::
  Core.AnnTerm PrimTy RawPrimVal ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
compileTerm (Core.Ann usage ty t) = case t of
  Core.Prim t' -> mkPrim t
  Core.AppM f xs -> mkApp (Core.term f) ty xs

mkPrim ::
  Core.Term PrimTy RawPrimVal ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkPrim (Core.Prim prim) = case prim of
  LitInt i -> return $ LLVM.int8 i -- TODO deal with the type correctly.

mkApp ::
  Core.Term PrimTy RawPrimVal ->
  Core.Type PrimTy ->
  [Core.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
mkApp f (Core.PrimTy ty) xs = case f of
  Core.Prim prim -> applyPrim prim ty xs

applyPrim ::
  RawPrimVal ->
  PrimTy ->
  [Core.AnnTerm PrimTy RawPrimVal] ->
  LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
applyPrim f ty xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm (xs P.!! 0)
        y <- compileTerm (xs P.!! 1)
        LLVM.add x y

typeToLLVM :: Core.Type PrimTy -> LLVM.Type
typeToLLVM (Core.PrimTy (PrimTy ty)) = ty

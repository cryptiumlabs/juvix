module Juvix.Backends.LLVM.Parameterization where

import qualified Juvix.Core.Parameterisation as Core
import Juvix.Library
import LLVM.AST

instance Core.CanApply Instruction where
  arity instr
    | isUnOp instr = 1
    | isBinOp instr = 2
    | isTernOp instr = 3
    | otherwise = case instr of
      Load {} -> 1
      Alloca {} -> 1
      Store {} -> 2
      GetElementPtr {} -> 1 + lengthN (indices instr)
      Fence {} -> 0
      CmpXchg {} -> 3
      AtomicRMW {} -> 2
      Phi {} -> 0 -- TODO: really 0?
      Call {} -> 0 -- TODO: really 0?
      VAArg {} -> 1
      LandingPad {} -> 0
      CatchPad {} -> 1 + lengthN (args instr)
      CleanupPad {} -> 1 + lengthN (args instr)

isUnOp :: Instruction -> Bool
isUnOp instr = case instr of
  Trunc {} -> True
  ZExt {} -> True
  SExt {} -> True
  FPToUI {} -> True
  FPToSI {} -> True
  UIToFP {} -> True
  SIToFP {} -> True
  FPTrunc {} -> True
  FPExt {} -> True
  PtrToInt {} -> True
  IntToPtr {} -> True
  BitCast {} -> True
  AddrSpaceCast {} -> True
  _ -> False

isBinOp :: Instruction -> Bool
isBinOp instr = case instr of
  Add {} -> True
  FAdd {} -> True
  Sub {} -> True
  FSub {} -> True
  Mul {} -> True
  FMul {} -> True
  UDiv {} -> True
  SDiv {} -> True
  FDiv {} -> True
  URem {} -> True
  SRem {} -> True
  FRem {} -> True
  Shl {} -> True
  LShr {} -> True
  AShr {} -> True
  And {} -> True
  Or {} -> True
  Xor {} -> True
  ICmp {} -> True
  FCmp {} -> True
  ExtractElement {} -> True
  InsertElement {} -> True
  ShuffleVector {} -> True
  ExtractValue {} -> True
  InsertValue {} -> True
  _ -> False

isTernOp :: Instruction -> Bool
isTernOp instr = case instr of
  Select {} -> True
  _ -> False

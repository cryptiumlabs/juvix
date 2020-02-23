-- |
-- - Serves as the AST for all operations on [[Nets/Default]]
-- - Resembles the =BOHM= syntax/operation set, however it alters the
--   operation set quite a bit
--   + _Added_
--     1. Notion of some kind of primitive value
--     2. Curry 1-3
--        - This allows custom functions to be manifested on primitive
--          values
--   + _Removed_
--     1. =Letrec=
--        - While here, it isn't fully implemented
--     2. Hard coded math operations
--        - This has been moved to primitive value operations
module Juvix.Interpreter.InteractionNet.Type where

import Juvix.Interpreter.InteractionNet.Shared
import Juvix.Library

data AST primVal
  = IntLit Int
  | Lambda Symbol (AST primVal)
  | Application (AST primVal) (AST primVal)
  | Not (AST primVal)
  | True'
  | False'
  | Letrec Symbol (AST primVal)
  | Let Symbol (AST primVal) (AST primVal)
  | If (AST primVal) (AST primVal) (AST primVal)
  | Cons (AST primVal) (AST primVal)
  | Or (AST primVal) (AST primVal)
  | And (AST primVal) (AST primVal)
  | Nil
  | Car (AST primVal)
  | Cdr (AST primVal)
  | IsNil (AST primVal)
  | Symbol' Symbol
  | -- Not valid syntax but for read back of a graph
    Erase
  | -- Not valid syntax but for read back of a graph
    Curried3
      (Primitive → Primitive → Primitive → Maybe Primitive)
      (AST primVal)
      (AST primVal)
      (AST primVal)
  | Curried2 (Primitive → Primitive → Maybe Primitive) (AST primVal) (AST primVal)
  | Curried1 (Primitive → Maybe Primitive) (AST primVal)
  | PrimCurried2 (primVal → primVal → Maybe primVal) (AST primVal) (AST primVal)
  | PrimCurried1 (primVal → Maybe primVal) (AST primVal)
  | Prim primVal
  deriving (Show)

-- | Constructs a Function from a primitive
-- the final argument is maybe, as if the nodes don't line up
-- a final type can't be constructed. This is untyped
-- so type check at a higher level
data Fn primVal
  = Arg0 Primitive
  | Arg1 (Primitive → Maybe Primitive)
  | Arg2 (Primitive → Primitive → Maybe Primitive)
  | Arg3 (Primitive → Primitive → Primitive → Maybe Primitive)
  deriving (Show, Generic)

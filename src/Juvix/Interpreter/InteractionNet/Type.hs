module Juvix.Interpreter.InteractionNet.Type where

import Juvix.Interpreter.InteractionNet.Shared
import Juvix.Library

-- TODO ∷ Investigate if it would be advantageous to promote this to a well typed gadt
data AST
  = IntLit Int
  | Lambda Symbol AST
  | Application AST AST
  | Not AST
  | True'
  | False'
  | Letrec Symbol AST
  | Let Symbol AST AST
  | If AST AST AST
  | Cons AST AST
  | Or AST AST
  | And AST AST
  | Nil
  | Car AST
  | Cdr AST
  | IsNil AST
  | Symbol' Symbol
  | -- Not valid syntax but for read back of a graph
    Erase
  | -- Not valid syntax but for read back of a graph
    Curried3 (Primitive → Primitive → Primitive → Maybe Primitive) AST AST AST
  | Curried2 (Primitive → Primitive → Maybe Primitive) AST AST
  | Curried1 (Primitive → Maybe Primitive) AST
  deriving (Show)

-- | Constructs a Function from a primitive
-- the final argument is maybe, as if the nodes don't line up
-- a final type can't be constructed. This is untyped
-- so type check at a higher level
data Fn
  = Arg0 Primitive
  | Arg1 (Primitive → Maybe Primitive)
  | Arg2 (Primitive → Primitive → Maybe Primitive)
  | Arg3 (Primitive → Primitive → Primitive → Maybe Primitive)
  deriving (Show, Generic)

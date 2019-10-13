module Juvix.Bohm.Type where

import Juvix.Bohm.Shared
import Juvix.Library

-- TODO∷ Investigate if it would be advantageous to promote this to a well typed gadt
data Bohm
  = IntLit Int
  | Lambda Symbol Bohm
  | Application Bohm Bohm
  | Not Bohm
  | True'
  | False'
  | Letrec Symbol Bohm
  | Let Symbol Bohm Bohm
  | If Bohm Bohm Bohm
  | Cons Bohm Bohm
  | Or Bohm Bohm
  | And Bohm Bohm
  | Nil
  | Car Bohm
  | Cdr Bohm
  | IsNil Bohm
  | Symbol' Symbol
  | -- Not valid syntax but for read back of a graph
    Erase
  | -- Not valid syntax but for read back of a graph
    Curried3 (Primitive -> Primitive -> Primitive -> Maybe Primitive) Bohm Bohm Bohm
  | Curried2 (Primitive -> Primitive -> Maybe Primitive) Bohm Bohm
  | Curried1 (Primitive -> Maybe Primitive) Bohm
  deriving (Show)

-- | Constructs a Function from a primitive
-- the final argument is maybe, as if the nodes don't line up
-- a final type can't be constructed. This is untyped
-- so type check at a higher level
data Fn
  = Arg0 Primitive
  | Arg1 (Primitive -> Maybe Primitive)
  | Arg2 (Primitive -> Primitive -> Maybe Primitive)
  | Arg3 (Primitive -> Primitive -> Primitive -> Maybe Primitive)
  deriving (Show, Generic)

-- |
-- - This module serves as a lower layer DSL that is just a binding
--   over the untyped instruction bindings
module Juvix.Backends.Michelson.DSL.Instructions where

import Juvix.Library
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Type as Type
import qualified Michelson.Untyped.Value as Value

car ∷ Instr.ExpandedOp
car = Instr.PrimEx (Instr.CAR "" "")


cdr ∷ Instr.ExpandedOp
cdr = Instr.PrimEx (Instr.CDR "" "")


dup ∷ Instr.ExpandedOp
dup = Instr.PrimEx (Instr.DUP "")

swap ∷ Instr.ExpandedOp
swap = Instr.PrimEx Instr.SWAP

dig ∷ Word → Instr.ExpandedOp
dig = Instr.PrimEx . Instr.DIG

dug ∷ Word → Instr.ExpandedOp
dug = Instr.PrimEx . Instr.DUG

push ∷ Type.Type → Value.Value' Instr.ExpandedOp → Instr.ExpandedOp
push typ value = Instr.PrimEx (Instr.PUSH "" typ value)

some ∷ Instr.ExpandedOp
some = Instr.PrimEx (Instr.SOME "" "")

unit ∷ Instr.ExpandedOp
unit = Instr.PrimEx (Instr.UNIT "" "")

pair ∷ Instr.ExpandedOp
pair = Instr.PrimEx (Instr.PAIR "" "" "" "")

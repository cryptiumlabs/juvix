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

left ∷ Type.Type → Instr.ExpandedOp
left = Instr.PrimEx . Instr.LEFT "" "" "" ""

right ∷ Type.Type → Instr.ExpandedOp
right = Instr.PrimEx . Instr.RIGHT "" "" "" ""

nil ∷ Type.Type → Instr.ExpandedOp
nil = Instr.PrimEx . Instr.NIL "" ""

cons ∷ Instr.ExpandedOp
cons = Instr.PrimEx (Instr.CONS "")

size ∷ Instr.ExpandedOp
size = Instr.PrimEx (Instr.SIZE "")

emptySet ∷ Type.Comparable → Instr.ExpandedOp
emptySet = Instr.PrimEx . Instr.EMPTY_SET "" ""

emptyMap ∷ Type.Comparable → Type.Type → Instr.ExpandedOp
emptyMap = (Instr.PrimEx .) . Instr.EMPTY_MAP "" ""

emptyBigMap ∷ Type.Comparable → Type.Type → Instr.ExpandedOp
emptyBigMap = (Instr.PrimEx .) . Instr.EMPTY_BIG_MAP "" ""

mem ∷ Instr.ExpandedOp
mem = Instr.PrimEx (Instr.MEM "")

get ∷ Instr.ExpandedOp
get = Instr.PrimEx (Instr.GET "")

update ∷ Instr.ExpandedOp
update = Instr.PrimEx (Instr.UPDATE "")

exec ∷ Instr.ExpandedOp
exec = Instr.PrimEx (Instr.EXEC "")

apply ∷ Instr.ExpandedOp
apply = Instr.PrimEx (Instr.APPLY "")

cast ∷ Type.Type → Instr.ExpandedOp
cast = Instr.PrimEx . Instr.CAST ""

rename ∷ Instr.ExpandedOp
rename = Instr.PrimEx (Instr.RENAME "")

pack ∷ Instr.ExpandedOp
pack = Instr.PrimEx (Instr.PACK "")

unpack ∷ Type.Type → Instr.ExpandedOp
unpack = Instr.PrimEx . Instr.UNPACK "" ""

concat ∷ Instr.ExpandedOp
concat = Instr.PrimEx (Instr.CONCAT "")

slice ∷ Instr.ExpandedOp
slice = Instr.PrimEx (Instr.SLICE "")

isNat ∷ Instr.ExpandedOp
isNat = Instr.PrimEx (Instr.ISNAT "")

add ∷ Instr.ExpandedOp
add = Instr.PrimEx (Instr.ADD "")

sub ∷ Instr.ExpandedOp
sub = Instr.PrimEx (Instr.SUB "")

mul ∷ Instr.ExpandedOp
mul = Instr.PrimEx (Instr.MUL "")

ediv ∷ Instr.ExpandedOp
ediv = Instr.PrimEx (Instr.EDIV "")

abs ∷ Instr.ExpandedOp
abs = Instr.PrimEx (Instr.ABS "")

neg ∷ Instr.ExpandedOp
neg = Instr.PrimEx (Instr.NEG "")

lsl ∷ Instr.ExpandedOp
lsl = Instr.PrimEx (Instr.LSL "")

lsr ∷ Instr.ExpandedOp
lsr = Instr.PrimEx (Instr.LSR "")

or ∷ Instr.ExpandedOp
or = Instr.PrimEx (Instr.OR "")

and ∷ Instr.ExpandedOp
and = Instr.PrimEx (Instr.AND "")

xor ∷ Instr.ExpandedOp
xor = Instr.PrimEx (Instr.XOR "")

not ∷ Instr.ExpandedOp
not = Instr.PrimEx (Instr.NOT "")

compare ∷ Instr.ExpandedOp
compare = Instr.PrimEx (Instr.COMPARE "")

eq ∷ Instr.ExpandedOp
eq = Instr.PrimEx (Instr.EQ "")

neq ∷ Instr.ExpandedOp
neq = Instr.PrimEx (Instr.NEQ "")

lt ∷ Instr.ExpandedOp
lt = Instr.PrimEx (Instr.LT "")

le ∷ Instr.ExpandedOp
le = Instr.PrimEx (Instr.LE "")

ge ∷ Instr.ExpandedOp
ge = Instr.PrimEx (Instr.GE "")

int ∷ Instr.ExpandedOp
int = Instr.PrimEx (Instr.INT "")

self ∷ Instr.ExpandedOp
self = Instr.PrimEx (Instr.SELF "")

contract ∷ Type.Type → Instr.ExpandedOp
contract = Instr.PrimEx . Instr.CONTRACT "" ""

transferTokens ∷ Instr.ExpandedOp
transferTokens = Instr.PrimEx (Instr.TRANSFER_TOKENS "")

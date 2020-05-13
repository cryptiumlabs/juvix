-- |
-- - This module serves as a lower layer DSL that is just a binding
--   over the untyped instruction bindings
module Juvix.Backends.Michelson.DSL.Instructions where

import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.DSL.Untyped as Untyped
import Juvix.Library
import qualified Michelson.Untyped.Contract as Contract
import qualified Michelson.Untyped.Ext as Ext
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Value as Value
import Prelude (error)

-- | 'toNewPrim' removes the implicit Instr.PrimEx from the instruction
-- and adds Inst over it, making it a new primitive. useful for making tests
toNewPrimErr :: Instr.ExpandedOp -> Types.NewPrim
toNewPrimErr (Instr.PrimEx x) =
  Types.Inst x
toNewPrimErr (Instr.SeqEx _) =
  error "sent in a Sequence of Instructions, but wanted a single"
toNewPrimErr (Instr.WithSrcEx _ _) =
  error "sent in a withsrcEx of Instructions, but wanted a single instruction"

ext :: Ext.ExtInstrAbstract Instr.ExpandedOp -> Instr.ExpandedOp
ext = Instr.PrimEx . Instr.EXT

drop :: Instr.ExpandedOp
drop = Instr.PrimEx Instr.DROP

dropN :: Word -> Instr.ExpandedOp
dropN = Instr.PrimEx . Instr.DROPN

car :: Instr.ExpandedOp
car = Instr.PrimEx (Instr.CAR "" "")

cdr :: Instr.ExpandedOp
cdr = Instr.PrimEx (Instr.CDR "" "")

dup :: Instr.ExpandedOp
dup = Instr.PrimEx (Instr.DUP "")

swap :: Instr.ExpandedOp
swap = Instr.PrimEx Instr.SWAP

dig :: Word -> Instr.ExpandedOp
dig = Instr.PrimEx . Instr.DIG

dug :: Word -> Instr.ExpandedOp
dug = Instr.PrimEx . Instr.DUG

push :: Untyped.T -> Value.Value' Instr.ExpandedOp -> Instr.ExpandedOp
push = Instr.PrimEx ... Instr.PUSH ""

some :: Instr.ExpandedOp
some = Instr.PrimEx (Instr.SOME "" "")

none :: Untyped.T -> Instr.ExpandedOp
none = Instr.PrimEx . Instr.NONE "" ""

unit :: Instr.ExpandedOp
unit = Instr.PrimEx (Instr.UNIT "" "")

pair :: Instr.ExpandedOp
pair = Instr.PrimEx (Instr.PAIR "" "" "" "")

left :: Untyped.T -> Instr.ExpandedOp
left = Instr.PrimEx . Instr.LEFT "" "" "" ""

right :: Untyped.T -> Instr.ExpandedOp
right = Instr.PrimEx . Instr.RIGHT "" "" "" ""

nil :: Untyped.T -> Instr.ExpandedOp
nil = Instr.PrimEx . Instr.NIL "" ""

cons :: Instr.ExpandedOp
cons = Instr.PrimEx (Instr.CONS "")

size :: Instr.ExpandedOp
size = Instr.PrimEx (Instr.SIZE "")

emptySet :: Untyped.T -> Instr.ExpandedOp
emptySet = Instr.PrimEx . Instr.EMPTY_SET "" ""

emptyMap :: Untyped.T -> Untyped.T -> Instr.ExpandedOp
emptyMap = Instr.PrimEx ... Instr.EMPTY_MAP "" ""

emptyBigMap :: Untyped.T -> Untyped.T -> Instr.ExpandedOp
emptyBigMap = Instr.PrimEx ... Instr.EMPTY_BIG_MAP "" ""

mem :: Instr.ExpandedOp
mem = Instr.PrimEx (Instr.MEM "")

get :: Instr.ExpandedOp
get = Instr.PrimEx (Instr.GET "")

update :: Instr.ExpandedOp
update = Instr.PrimEx (Instr.UPDATE "")

exec :: Instr.ExpandedOp
exec = Instr.PrimEx (Instr.EXEC "")

apply :: Instr.ExpandedOp
apply = Instr.PrimEx (Instr.APPLY "")

cast :: Untyped.T -> Instr.ExpandedOp
cast = Instr.PrimEx . Instr.CAST ""

rename :: Instr.ExpandedOp
rename = Instr.PrimEx (Instr.RENAME "")

pack :: Instr.ExpandedOp
pack = Instr.PrimEx (Instr.PACK "")

unpack :: Untyped.T -> Instr.ExpandedOp
unpack = Instr.PrimEx . Instr.UNPACK "" ""

concat :: Instr.ExpandedOp
concat = Instr.PrimEx (Instr.CONCAT "")

slice :: Instr.ExpandedOp
slice = Instr.PrimEx (Instr.SLICE "")

isNat :: Instr.ExpandedOp
isNat = Instr.PrimEx (Instr.ISNAT "")

add :: Instr.ExpandedOp
add = Instr.PrimEx (Instr.ADD "")

sub :: Instr.ExpandedOp
sub = Instr.PrimEx (Instr.SUB "")

mul :: Instr.ExpandedOp
mul = Instr.PrimEx (Instr.MUL "")

ediv :: Instr.ExpandedOp
ediv = Instr.PrimEx (Instr.EDIV "")

abs :: Instr.ExpandedOp
abs = Instr.PrimEx (Instr.ABS "")

neg :: Instr.ExpandedOp
neg = Instr.PrimEx (Instr.NEG "")

lsl :: Instr.ExpandedOp
lsl = Instr.PrimEx (Instr.LSL "")

lsr :: Instr.ExpandedOp
lsr = Instr.PrimEx (Instr.LSR "")

or :: Instr.ExpandedOp
or = Instr.PrimEx (Instr.OR "")

and :: Instr.ExpandedOp
and = Instr.PrimEx (Instr.AND "")

xor :: Instr.ExpandedOp
xor = Instr.PrimEx (Instr.XOR "")

not :: Instr.ExpandedOp
not = Instr.PrimEx (Instr.NOT "")

compare :: Instr.ExpandedOp
compare = Instr.PrimEx (Instr.COMPARE "")

eq :: Instr.ExpandedOp
eq = Instr.PrimEx (Instr.EQ "")

neq :: Instr.ExpandedOp
neq = Instr.PrimEx (Instr.NEQ "")

lt :: Instr.ExpandedOp
lt = Instr.PrimEx (Instr.LT "")

le :: Instr.ExpandedOp
le = Instr.PrimEx (Instr.LE "")

ge :: Instr.ExpandedOp
ge = Instr.PrimEx (Instr.GE "")

gt :: Instr.ExpandedOp
gt = Instr.PrimEx (Instr.GT "")

int :: Instr.ExpandedOp
int = Instr.PrimEx (Instr.INT "")

self :: Instr.ExpandedOp
self = Instr.PrimEx (Instr.SELF "" "")

contract :: Untyped.T -> Instr.ExpandedOp
contract = Instr.PrimEx . Instr.CONTRACT "" ""

transferTokens :: Instr.ExpandedOp
transferTokens = Instr.PrimEx (Instr.TRANSFER_TOKENS "")

setDelegate :: Instr.ExpandedOp
setDelegate = Instr.PrimEx (Instr.SET_DELEGATE "")

createContract :: Contract.Contract' Instr.ExpandedOp -> Instr.ExpandedOp
createContract = Instr.PrimEx . Instr.CREATE_CONTRACT "" ""

implicitAccount :: Instr.ExpandedOp
implicitAccount = Instr.PrimEx (Instr.IMPLICIT_ACCOUNT "")

now :: Instr.ExpandedOp
now = Instr.PrimEx (Instr.NOW "")

amount :: Instr.ExpandedOp
amount = Instr.PrimEx (Instr.AMOUNT "")

balance :: Instr.ExpandedOp
balance = Instr.PrimEx (Instr.BALANCE "")

checkSignature :: Instr.ExpandedOp
checkSignature = Instr.PrimEx (Instr.CHECK_SIGNATURE "")

sha256 :: Instr.ExpandedOp
sha256 = Instr.PrimEx (Instr.SHA256 "")

sha512 :: Instr.ExpandedOp
sha512 = Instr.PrimEx (Instr.SHA512 "")

blake2b :: Instr.ExpandedOp
blake2b = Instr.PrimEx (Instr.BLAKE2B "")

hashKey :: Instr.ExpandedOp
hashKey = Instr.PrimEx (Instr.HASH_KEY "")

source :: Instr.ExpandedOp
source = Instr.PrimEx (Instr.SOURCE "")

address :: Instr.ExpandedOp
address = Instr.PrimEx (Instr.ADDRESS "")

chainID :: Instr.ExpandedOp
chainID = Instr.PrimEx (Instr.CHAIN_ID "")

ifNone :: [Instr.ExpandedOp] -> [Instr.ExpandedOp] -> Instr.ExpandedOp
ifNone = Instr.PrimEx ... Instr.IF_NONE

ifLeft :: [Instr.ExpandedOp] -> [Instr.ExpandedOp] -> Instr.ExpandedOp
ifLeft = Instr.PrimEx ... Instr.IF_LEFT

if' :: [Instr.ExpandedOp] -> [Instr.ExpandedOp] -> Instr.ExpandedOp
if' = Instr.PrimEx ... Instr.IF

map :: [Instr.ExpandedOp] -> Instr.ExpandedOp
map = Instr.PrimEx . Instr.MAP ""

iter :: [Instr.ExpandedOp] -> Instr.ExpandedOp
iter = Instr.PrimEx . Instr.ITER

loop :: [Instr.ExpandedOp] -> Instr.ExpandedOp
loop = Instr.PrimEx . Instr.LOOP

loopLeft :: [Instr.ExpandedOp] -> Instr.ExpandedOp
loopLeft = Instr.PrimEx . Instr.LOOP_LEFT

lambda :: Untyped.T -> Untyped.T -> [Instr.ExpandedOp] -> Instr.ExpandedOp
lambda = (Instr.PrimEx .) ... Instr.LAMBDA ""

dip :: [Instr.ExpandedOp] -> Instr.ExpandedOp
dip = Instr.PrimEx . Instr.DIP

dipN :: Word -> [Instr.ExpandedOp] -> Instr.ExpandedOp
dipN = Instr.PrimEx ... Instr.DIPN

instance Semigroup Instr.ExpandedOp where
  Instr.SeqEx xs <> Instr.SeqEx ys =
    Instr.SeqEx (xs <> ys)
  Instr.SeqEx xs <> y = Instr.SeqEx (xs <> [y])
  x <> Instr.SeqEx ys = Instr.SeqEx (x : ys)
  x <> y = Instr.SeqEx [x, y]

instance Monoid Instr.ExpandedOp where
  mempty = Instr.SeqEx []

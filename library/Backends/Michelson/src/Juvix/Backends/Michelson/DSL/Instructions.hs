{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

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
toNewPrimErr :: Instr.ExpandedOp -> Types.RawPrimVal
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
car = Instr.PrimEx (Instr.CAR Untyped.blank Untyped.blank)

cdr :: Instr.ExpandedOp
cdr = Instr.PrimEx (Instr.CDR Untyped.blank Untyped.blank)

dup :: Instr.ExpandedOp
dup = Instr.PrimEx (Instr.DUP Untyped.blank)

swap :: Instr.ExpandedOp
swap = Instr.PrimEx Instr.SWAP

dig :: Word -> Instr.ExpandedOp
dig = Instr.PrimEx . Instr.DIG

dug :: Word -> Instr.ExpandedOp
dug = Instr.PrimEx . Instr.DUG

push :: Untyped.T -> Value.Value' Instr.ExpandedOp -> Instr.ExpandedOp
push = Instr.PrimEx ... Instr.PUSH Untyped.blank

some :: Instr.ExpandedOp
some = Instr.PrimEx (Instr.SOME Untyped.blank Untyped.blank)

none :: Untyped.T -> Instr.ExpandedOp
none = Instr.PrimEx . Instr.NONE Untyped.blank Untyped.blank

unit :: Instr.ExpandedOp
unit = Instr.PrimEx (Instr.UNIT Untyped.blank Untyped.blank)

pair :: Instr.ExpandedOp
pair = Instr.PrimEx (Instr.PAIR Untyped.blank Untyped.blank Untyped.blank Untyped.blank)

left :: Untyped.T -> Instr.ExpandedOp
left = Instr.PrimEx . Instr.LEFT Untyped.blank Untyped.blank Untyped.blank Untyped.blank

right :: Untyped.T -> Instr.ExpandedOp
right = Instr.PrimEx . Instr.RIGHT Untyped.blank Untyped.blank Untyped.blank Untyped.blank

nil :: Untyped.T -> Instr.ExpandedOp
nil = Instr.PrimEx . Instr.NIL Untyped.blank Untyped.blank

cons :: Instr.ExpandedOp
cons = Instr.PrimEx (Instr.CONS Untyped.blank)

size :: Instr.ExpandedOp
size = Instr.PrimEx (Instr.SIZE Untyped.blank)

emptySet :: Untyped.T -> Instr.ExpandedOp
emptySet = Instr.PrimEx . Instr.EMPTY_SET Untyped.blank Untyped.blank

emptyMap :: Untyped.T -> Untyped.T -> Instr.ExpandedOp
emptyMap = Instr.PrimEx ... Instr.EMPTY_MAP Untyped.blank Untyped.blank

emptyBigMap :: Untyped.T -> Untyped.T -> Instr.ExpandedOp
emptyBigMap = Instr.PrimEx ... Instr.EMPTY_BIG_MAP Untyped.blank Untyped.blank

mem :: Instr.ExpandedOp
mem = Instr.PrimEx (Instr.MEM Untyped.blank)

get :: Instr.ExpandedOp
get = Instr.PrimEx (Instr.GET Untyped.blank)

update :: Instr.ExpandedOp
update = Instr.PrimEx (Instr.UPDATE Untyped.blank)

exec :: Instr.ExpandedOp
exec = Instr.PrimEx (Instr.EXEC Untyped.blank)

apply :: Instr.ExpandedOp
apply = Instr.PrimEx (Instr.APPLY Untyped.blank)

cast :: Untyped.T -> Instr.ExpandedOp
cast = Instr.PrimEx . Instr.CAST Untyped.blank

rename :: Instr.ExpandedOp
rename = Instr.PrimEx (Instr.RENAME Untyped.blank)

pack :: Instr.ExpandedOp
pack = Instr.PrimEx (Instr.PACK Untyped.blank)

unpack :: Untyped.T -> Instr.ExpandedOp
unpack = Instr.PrimEx . Instr.UNPACK Untyped.blank Untyped.blank

concat :: Instr.ExpandedOp
concat = Instr.PrimEx (Instr.CONCAT Untyped.blank)

slice :: Instr.ExpandedOp
slice = Instr.PrimEx (Instr.SLICE Untyped.blank)

isNat :: Instr.ExpandedOp
isNat = Instr.PrimEx (Instr.ISNAT Untyped.blank)

add :: Instr.ExpandedOp
add = Instr.PrimEx (Instr.ADD Untyped.blank)

sub :: Instr.ExpandedOp
sub = Instr.PrimEx (Instr.SUB Untyped.blank)

mul :: Instr.ExpandedOp
mul = Instr.PrimEx (Instr.MUL Untyped.blank)

ediv :: Instr.ExpandedOp
ediv = Instr.PrimEx (Instr.EDIV Untyped.blank)

abs :: Instr.ExpandedOp
abs = Instr.PrimEx (Instr.ABS Untyped.blank)

neg :: Instr.ExpandedOp
neg = Instr.PrimEx (Instr.NEG Untyped.blank)

lsl :: Instr.ExpandedOp
lsl = Instr.PrimEx (Instr.LSL Untyped.blank)

lsr :: Instr.ExpandedOp
lsr = Instr.PrimEx (Instr.LSR Untyped.blank)

or :: Instr.ExpandedOp
or = Instr.PrimEx (Instr.OR Untyped.blank)

and :: Instr.ExpandedOp
and = Instr.PrimEx (Instr.AND Untyped.blank)

xor :: Instr.ExpandedOp
xor = Instr.PrimEx (Instr.XOR Untyped.blank)

not :: Instr.ExpandedOp
not = Instr.PrimEx (Instr.NOT Untyped.blank)

compare :: Instr.ExpandedOp
compare = Instr.PrimEx (Instr.COMPARE Untyped.blank)

eq :: Instr.ExpandedOp
eq = Instr.PrimEx (Instr.EQ Untyped.blank)

neq :: Instr.ExpandedOp
neq = Instr.PrimEx (Instr.NEQ Untyped.blank)

lt :: Instr.ExpandedOp
lt = Instr.PrimEx (Instr.LT Untyped.blank)

le :: Instr.ExpandedOp
le = Instr.PrimEx (Instr.LE Untyped.blank)

ge :: Instr.ExpandedOp
ge = Instr.PrimEx (Instr.GE Untyped.blank)

gt :: Instr.ExpandedOp
gt = Instr.PrimEx (Instr.GT Untyped.blank)

int :: Instr.ExpandedOp
int = Instr.PrimEx (Instr.INT Untyped.blank)

self :: Instr.ExpandedOp
self = Instr.PrimEx (Instr.SELF Untyped.blank Untyped.blank)

contract :: Untyped.T -> Instr.ExpandedOp
contract = Instr.PrimEx . Instr.CONTRACT Untyped.blank Untyped.blank

transferTokens :: Instr.ExpandedOp
transferTokens = Instr.PrimEx (Instr.TRANSFER_TOKENS Untyped.blank)

setDelegate :: Instr.ExpandedOp
setDelegate = Instr.PrimEx (Instr.SET_DELEGATE Untyped.blank)

createContract :: Contract.Contract' Instr.ExpandedOp -> Instr.ExpandedOp
createContract = Instr.PrimEx . Instr.CREATE_CONTRACT Untyped.blank Untyped.blank

implicitAccount :: Instr.ExpandedOp
implicitAccount = Instr.PrimEx (Instr.IMPLICIT_ACCOUNT Untyped.blank)

now :: Instr.ExpandedOp
now = Instr.PrimEx (Instr.NOW Untyped.blank)

amount :: Instr.ExpandedOp
amount = Instr.PrimEx (Instr.AMOUNT Untyped.blank)

balance :: Instr.ExpandedOp
balance = Instr.PrimEx (Instr.BALANCE Untyped.blank)

checkSignature :: Instr.ExpandedOp
checkSignature = Instr.PrimEx (Instr.CHECK_SIGNATURE Untyped.blank)

sha256 :: Instr.ExpandedOp
sha256 = Instr.PrimEx (Instr.SHA256 Untyped.blank)

sha512 :: Instr.ExpandedOp
sha512 = Instr.PrimEx (Instr.SHA512 Untyped.blank)

blake2b :: Instr.ExpandedOp
blake2b = Instr.PrimEx (Instr.BLAKE2B Untyped.blank)

hashKey :: Instr.ExpandedOp
hashKey = Instr.PrimEx (Instr.HASH_KEY Untyped.blank)

source :: Instr.ExpandedOp
source = Instr.PrimEx (Instr.SOURCE Untyped.blank)

address :: Instr.ExpandedOp
address = Instr.PrimEx (Instr.ADDRESS Untyped.blank)

chainID :: Instr.ExpandedOp
chainID = Instr.PrimEx (Instr.CHAIN_ID Untyped.blank)

ifNone :: [Instr.ExpandedOp] -> [Instr.ExpandedOp] -> Instr.ExpandedOp
ifNone = Instr.PrimEx ... Instr.IF_NONE

ifLeft :: [Instr.ExpandedOp] -> [Instr.ExpandedOp] -> Instr.ExpandedOp
ifLeft = Instr.PrimEx ... Instr.IF_LEFT

if' :: [Instr.ExpandedOp] -> [Instr.ExpandedOp] -> Instr.ExpandedOp
if' = Instr.PrimEx ... Instr.IF

map :: [Instr.ExpandedOp] -> Instr.ExpandedOp
map = Instr.PrimEx . Instr.MAP Untyped.blank

iter :: [Instr.ExpandedOp] -> Instr.ExpandedOp
iter = Instr.PrimEx . Instr.ITER

loop :: [Instr.ExpandedOp] -> Instr.ExpandedOp
loop = Instr.PrimEx . Instr.LOOP

loopLeft :: [Instr.ExpandedOp] -> Instr.ExpandedOp
loopLeft = Instr.PrimEx . Instr.LOOP_LEFT

lambda :: Untyped.T -> Untyped.T -> [Instr.ExpandedOp] -> Instr.ExpandedOp
lambda = (Instr.PrimEx .) ... Instr.LAMBDA Untyped.blank

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

toNumArgs :: Instr.InstrAbstract op -> Natural
toNumArgs x =
  case x of
    Instr.ADD _ -> 2
    Instr.SUB _ -> 2
    Instr.MUL _ -> 2
    Instr.OR {} -> 2
    Instr.AND _ -> 2
    Instr.XOR _ -> 2
    Instr.EQ {} -> 1
    Instr.NEQ _ -> 1
    Instr.LT {} -> 1
    Instr.LE {} -> 1
    Instr.GE {} -> 1
    Instr.GT {} -> 1
    Instr.NEG _ -> 1
    Instr.ABS _ -> 1
    Instr.CAR {} -> 1
    Instr.CDR {} -> 1
    Instr.PAIR {} -> 2
    Instr.EDIV _ -> 2
    Instr.ISNAT _ -> 1
    Instr.PUSH {} -> 1
    Instr.IF {} -> 3
    Instr.NONE {} -> 0
    Instr.EMPTY_SET {} -> 0
    Instr.EMPTY_MAP {} -> 0
    Instr.EMPTY_BIG_MAP {} -> 0
    Instr.NIL {} -> 0
    Instr.CONS {} -> 2
    Instr.CONTRACT {} -> 1

-- _ -> error "function not implemented yet"

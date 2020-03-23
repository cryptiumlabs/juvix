module Juvix.Backends.Michelson.DSL.Untyped where

import Juvix.Library ((.), (...), flip)
import qualified Michelson.Untyped.Type as Untyped

type T = Untyped.Type

type UnAnn = Untyped.T

type CT = Untyped.CT

type Comparable = Untyped.Comparable

--------------------------------------------------------------------------------
-- Annotated promotion
--------------------------------------------------------------------------------

annotate ∷ UnAnn → T
annotate = flip Untyped.Type ""

annComp ∷ CT → Comparable
annComp = flip Untyped.Comparable ""

--------------------------------------------------------------------------------
-- T abstractions
--------------------------------------------------------------------------------

key ∷ T
key = annotate Untyped.TKey

unit ∷ T
unit = annotate Untyped.TUnit

signature' ∷ T
signature' = annotate Untyped.TSignature

chainId ∷ T
chainId = annotate Untyped.TChainId

option ∷ T → T
option = annotate . Untyped.TOption

list ∷ T → T
list = annotate . Untyped.TList

set ∷ Comparable → T
set = annotate . Untyped.TSet

operation ∷ T
operation = annotate Untyped.TOperation

contract ∷ T → T
contract = annotate . Untyped.TContract

pair ∷ T → T → T
pair = annotate ... Untyped.TPair "" ""

or ∷ T → T → T
or = annotate ... Untyped.TOr "" ""

lambda ∷ T → T → T
lambda = annotate ... Untyped.TLambda

map ∷ Comparable → T → T
map = annotate ... Untyped.TMap

bigMap ∷ Comparable → T → T
bigMap = annotate ... Untyped.TBigMap

tc ∷ CT → T
tc = annotate . Untyped.Tc

--------------------------------------------------------------------------------
-- Constants May have to promote with either
--------------------------------------------------------------------------------

int, nat, string, bytes, mutez, bool, keyhash, timestamp, address ∷ CT
int = Untyped.CInt
nat = Untyped.CNat
bool = Untyped.CBool
bytes = Untyped.CBytes
mutez = Untyped.CMutez
string = Untyped.CString
keyhash = Untyped.CKeyHash
address = Untyped.CAddress
timestamp = Untyped.CTimestamp

module Tezos

import public Interfaces
import public Basics

import public Tezos.Prim

%access public export
%default total

namespace Map

  empty : Map k v
  empty = prim__tezosEmptyMap

  get : k -> Map k v -> Maybe v
  get = prim__tezosMapGet

  update : k -> Maybe v -> Map k v -> Map k v
  update = prim__tezosMapUpdate

  insert : k -> v -> Map k v -> Map k v
  insert k v m = update k (Just v) m

  member : k -> Map k v -> Bool
  member = prim__tezosMapMember

  size : Map k v -> Integer
  size = prim__tezosMapSize

  foldl : (a -> v -> a) -> a -> Map k v -> a
  foldl = prim__tezosFail

namespace Set

  empty : Set v
  empty = prim__tezosEmptySet

  member : v -> Set v -> Bool
  member = prim__tezosSetMember

  size : Set v -> Integer
  size = prim__tezosSetSize

amount : Tez
amount = prim__tezosAmount

now : Timestamp
now = prim__tezosNow

fail : a
fail = prim__tezosFail

sender : Address
sender = prim__tezosSender

sha256 : Bytes -> Bytes
sha256 = prim__tezosSHA256

transferTokens : p -> Tez -> Contract p -> Operation
transferTokens = prim__tezosTransferTokens

setDelegate : Maybe KeyHash -> Operation
setDelegate = prim__tezosSetDelegate

checkSignature : Key -> Signature -> Bytes -> Bool
checkSignature = prim__tezosCheckSignature

Eq String where
  (==) x y = prim__tezosEq (prim__tezosCompareString x y)

Eq Integer where
  (==) x y = prim__tezosEq (prim__tezosCompareInt x y)

Eq Nat where
  (==) x y = prim__tezosEq (prim__tezosCompareNat x y)

Eq Tez where
  (==) x y = prim__tezosEq (prim__tezosCompareTez x y)

Eq Address where
  (==) x y = prim__tezosEq (prim__tezosCompareAddress x y)

Eq Bytes where
  (==) x y = prim__tezosEq (prim__tezosCompareBytes x y)

Eq a => Eq (Maybe a) where
  (==) Nothing  Nothing   = True
  (==) (Just x) (Just y)  = (==) x y
  (==) _        _         = False

Num Integer where
  (+) = prim__tezosAddIntInt
  (*) = prim__tezosMulIntInt
  fromInteger = id

Num Nat where
  (+) = prim__tezosAddNatNat
  (*) = prim__tezosMulNatNat
  fromInteger = prim__tezosFail

Neg Nat where
  negate = prim__tezosFail
  (-) = prim__tezosSubNatNat

Ord Integer where
  (<) x y = prim__tezosLt (prim__tezosCompareInt x y)
  (>) x y = prim__tezosGt (prim__tezosCompareInt x y)

Ord Nat where
  (<) x y = prim__tezosLt (prim__tezosCompareNat x y)
  (>) x y = prim__tezosGt (prim__tezosCompareNat x y)

Ord Tez where
  (<) x y = prim__tezosLt (prim__tezosCompareTez x y)
  (>) x y = prim__tezosGt (prim__tezosCompareTez x y)

Semigroup String where
  (<+>) = prim__tezosConcatString

Semigroup Bytes where
  (<+>) = prim__tezosConcatBytes

ifThenElse : (b : Bool) -> (t : Lazy a) -> (e : Lazy a) -> a
ifThenElse True  t e = t
ifThenElse False t e = e

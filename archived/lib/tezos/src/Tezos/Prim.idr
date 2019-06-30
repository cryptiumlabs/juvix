module Tezos.Prim

%access public export
%default total

-- Datatypes

data Bool = False | True

data Key

data KeyHash

data Address

data Hash

data Integer

data Nat

data Timestamp

data Tez

data String

data Bytes

data Signature

data Operation

data Contract : (p : Type) -> Type

data Either : (l : Type, r : Type) -> Type where
  Left  : l -> Either l r
  Right : r -> Either l r

data Map : (k : Type, v : Type) -> Type

data BigMap : (k : Type, v : Type) -> Type

data Set : (v : Type) -> Type

data List : (e : Type) -> Type where
  Nil   : List e
  Cons  : e -> List e -> List e

data Maybe : (v : Type) -> Type where
  Nothing : Maybe v
  Just    : (x : v) -> Maybe v

-- Primitives
-- In order of Michelson spec listing.

%extern prim__tezosConsPair : (x : a) -> (y : b) -> Pair a b

%extern prim__tezosListMap : (f : a -> b) -> List a -> List b

%extern prim__tezosListReduce : ()

%extern prim__tezosEmptySet : Set a

%extern prim__tezosSetMap : (f : a -> b) -> Set a -> Set b

%extern prim__tezosSetMember : v -> Set v -> Bool

%extern prim__tezosSetSize : Set v -> Integer

%extern prim__tezosEmptyMap : Map k v

%extern prim__tezosMapGet : k -> Map k v -> Maybe v

%extern prim__tezosMapMember : k -> Map k v -> Bool

%extern prim__tezosMapUpdate : k -> Maybe v -> Map k v -> Map k v

%extern prim__tezosMapSize : Map k v -> Integer

%extern prim__tezosEq : Integer -> Bool

%extern prim__tezosNeq : Integer -> Bool

%extern prim__tezosLt : Integer -> Bool

%extern prim__tezosGt : Integer -> Bool

%extern prim__tezosLe : Integer -> Bool

%extern prim__tezosGe : Integer -> Bool

%extern prim__tezosOr : Bool -> Bool -> Bool

%extern prim__tezosAnd : Bool -> Bool -> Bool

%extern prim__tezosXor : Bool -> Bool -> Bool

%extern prim__tezosNot : Bool -> Bool -> Bool

%extern prim__tezosNegInt : Integer -> Integer

%extern prim__tezosNegNat : Nat -> Nat

%extern prim__tezosAbs : Integer -> Nat

%extern prim__tezosAddIntInt : Integer -> Integer -> Integer

%extern prim__tezosAddNatNat : Nat -> Nat -> Nat

%extern prim__tezosSubIntInt : Integer -> Integer -> Integer

%extern prim__tezosSubNatNat : Nat -> Nat -> Nat

%extern prim__tezosMulIntInt : Integer -> Integer -> Integer

%extern prim__tezosMulNatNat : Nat -> Nat -> Nat

%extern prim__tezosEdivIntInt : Integer -> Integer -> Maybe (Integer, Nat)

%extern prim__tezosEdivNatNat : Nat -> Nat -> Maybe (Nat, Nat)

%extern prim__tezosCompareInt : Integer -> Integer -> Integer

%extern prim__tezosCompareNat : Nat -> Nat -> Integer

%extern prim__tezosConcatString : String -> String -> String

%extern prim__tezosSizeString : String -> Nat

%extern prim__tezosCompareTez : Tez -> Tez -> Integer

%extern prim__tezosCompareString : String -> String -> Integer

%extern prim__tezosCompareAddress : Address -> Address -> Integer

%extern prim__tezosCompareBytes : Bytes -> Bytes -> Integer

%extern prim__tezosFail : a

%extern prim__tezosFailWith : a -> b

%extern prim__tezosExec : a -> (a -> b) -> b

%extern prim__tezosLambda : (a -> b) -> (a -> b)

%extern prim__tezosTransferTokens : p -> Tez -> Contract p -> Operation

%extern prim__tezosSetDelegate : Maybe KeyHash -> Operation

%extern prim__tezosAddress : Contract p -> Address

%extern prim__tezosContract : Address -> Maybe (Contract p)

%extern prim__tezosSource : Address

%extern prim__tezosSender : Address

%extern prim__tezosSelf : Contract p

%extern prim__tezosImplicitAccount : KeyHash -> Contract Unit

%extern prim__tezosManager : Key

%extern prim__tezosNow : Timestamp

%extern prim__tezosBalance : Tez

%extern prim__tezosAmount : Tez

%extern prim__tezosHashKey : Key -> KeyHash

%extern prim__tezosConcatBytes : Bytes -> Bytes -> Bytes

%extern prim__tezosBlake2B : Bytes -> Bytes

%extern prim__tezosSHA256 : Bytes -> Bytes

%extern prim__tezosSHA512 : Bytes -> Bytes

%extern prim__tezosCheckSignature : Key -> Signature -> Bytes -> Bool

-- Syntax rules

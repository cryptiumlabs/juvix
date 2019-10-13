module Juvix.Backends.Michelson.Untyped where

import Juvix.Utility
import Protolude hiding (Const (..), Type (..))

type Stack = [(StackElem, Type)]

data StackElem
  = ConstE Const
  | VarE Text
  | FuncResult
  deriving (Show, Eq, PrettyPrint)

data Type
  = UnitT
  | KeyT
  | HashT
  | IntT
  | TezT
  | BoolT
  | StringT
  | OperationT
  | EitherT Type Type
  | OptionT Type
  | ListT Type
  | PairT Type Type
  | LamT Type Type
  deriving (Show, Eq, PrettyPrint)

data Const
  = Unit
  | String Text
  | Bool Bool
  | Tez Integer
  | Int Integer
  deriving (Show, Eq, PrettyPrint)

data Expr
  = {- Stack Operations -}

    Drop
  | Dup
  | Swap
  | Const Const
  | {- Pairs -}

    ConsPair
  | Car
  | Cdr
  | {- Options -}

    ConsSome
  | ConsNone
  | IfNone Expr Expr
  | {- Unions -}

    Left
  | Right
  | IfLeft Expr Expr
  | {- Lists -}

    ConsList
  | Nil Type
  | IfCons Expr Expr
  | ListMap
  | ListReduce
  | {- Sets -}

    EmptySet
  | SetMap
  | SetReduce
  | SetMem
  | SetUpdate
  | SetSize
  | {- Maps -}

    EmptyMap
  | MapMap
  | MapReduce
  | MapMem
  | MapGet
  | MapUpdate
  | MapSize
  | {- String Operations -}

    Concat
  | {- Timestamp Operations -}

    AddSecondsToTimestamp
  | AddTimestampToSeconds
  | {- Currency Operations -}

    AddTez
  | SubTez
  | MulTezNat
  | MulNatTez
  | EdivTezNat
  | EdivTez
  | {- Boolean Operations -}

    Or
  | And
  | Xor
  | Not
  | {- Integer Operations -}

    NegNat
  | NegInt
  | AbsNat
  | AbsInt
  | IntNat
  | AddIntInt
  | AddIntNat
  | AddNatInt
  | AddNatNat
  | SubInt
  | MulIntInt
  | MulIntNat
  | MulNatInt
  | MulNatNat
  | EdivIntInt
  | EdivIntNat
  | EdivNatInt
  | EdivNatNat
  | LslNat
  | LsrNat
  | OrNat
  | AndNat
  | XorNat
  | NotNat
  | NotInt
  | {- Control -}

    Seq Expr Expr
  | If Expr Expr
  | Loop Expr
  | Dip Expr
  | Exec
  | Lambda Expr
  | Fail
  | Nop
  | {- Comparision -}

    Compare
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | {- Protocol -}

    Manager
  | TransferTokens
  | CreateAccount
  | DefaultAccount
  | CreateContract
  | Now
  | Balance
  | CheckSignature
  | HashKey
  | H
  | StepsToQuota
  | Source
  | Amount
  deriving (Show, Eq, PrettyPrint)

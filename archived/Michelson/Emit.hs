module Juvix.Backends.Michelson.Emit where

import qualified Data.Text                        as T
import           Data.Typeable
import           Protolude                        hiding (Const (..), Left,
                                                   Right)
import qualified Type.Reflection                  as R

import           Juvix.Backends.Michelson.Typed
import qualified Juvix.Backends.Michelson.Untyped as U

emit ∷ ∀ a b . (R.Typeable a, R.Typeable b) ⇒ Expr a b → T.Text
emit expr =
  case expr of

    Drop     → "DROP"
    Dup      → "DUP"
    Swap     → "SWAP"
    Const (v ∷ t) ->
      case eqT ∷ Maybe (t :~: ()) of
        Just Refl → "UNIT"
        _ ->
          case eqT ∷ Maybe (t :~: Integer) of
            Just Refl → T.concat ["PUSH int ", show v]
            _         →
              case eqT ∷ Maybe (t :~: T.Text) of
                Just Refl → T.concat ["PUSH string ", show v]
                _         →
                  case eqT ∷ Maybe (t :~: Bool) of
                    Just Refl → T.concat ["PUSH bool ", show v]
                    _         → "e"


    ConsPair → "PAIR"
    Car      → "CAR"
    Cdr      → "CDR"

    Left        → "LEFT"
    Right       → "RIGHT"
    IfLeft a b  → T.concat ["IF_LEFT {", emit a, "} {", emit b, "}"]

    -- TODO
    Nil      → "NIL operation"

    AddIntInt -> "ADD"
    MulIntInt -> "MUL"

    Amount   → "AMOUNT"

    Seq a b  → T.concat ["{", emit a, "; ", emit b, "}"]
    If a b   → T.concat ["IF {", emit a, "} {", emit b, "}"]
    Dip a    → T.concat ["DIP {", emit a, "}"]
    Exec     → "EXEC"
    Nop      → "NOP"

    _        → "e"

emitUntyped ∷ U.Expr → Text
emitUntyped expr =
  case expr of

    U.Drop       → "DROP"
    U.Dup        → "DUP"
    U.Swap       → "SWAP"
    U.Const U.Unit        → "UNIT"
    U.Const (U.Bool b)    → T.concat ["PUSH bool ", if b then "True" else "False"]
    U.Const (U.Int i)     → T.concat ["PUSH int ", show i]
    U.Const (U.Tez i)     → T.concat ["PUSH tez \"", show i, "\""]
    U.Const (U.String s)  → T.concat ["\"", s, "\""]

    U.ConsPair   → "PAIR"
    U.Car        → "CAR"
    U.Cdr        → "CDR"

    U.ConsSome   → "SOME"
    U.ConsNone   → "NONE"
    U.IfNone a b → T.concat ["IF_NONE {", emitUntyped a, "} {", emitUntyped b, "}"]

    U.Left       → "LEFT"
    U.Right      → "RIGHT"
    U.IfLeft a b → T.concat ["IF_LEFT {", emitUntyped a, "} {", emitUntyped b, "}"]

    U.ConsList   → "CONS"
    U.Nil ty     → "NIL " <> emitType ty
    U.IfCons a b → T.concat ["IF_CONS {", emitUntyped a, "} {", emitUntyped b, "}"]
    U.ListMap    → "MAP"
    U.ListReduce → "REDUCE"

    U.EmptySet   → "EMPTY_SET"
    U.SetMap     → "MAP"
    U.SetReduce  → "REDUCE"
    U.SetMem     → "MEM"
    U.SetUpdate  → "UPDATE"
    U.SetSize    → "SIZE"

    U.EmptyMap   → "EMPTY_MAP"
    U.MapMap     → "MAP"
    U.MapReduce  → "REDUCE"
    U.MapMem     → "MEM"
    U.MapGet     → "GET"
    U.MapUpdate  → "UPDATE"
    U.MapSize    → "SIZE"

    U.Concat     → "CONCAT"

    U.AddSecondsToTimestamp → "ADD"
    U.AddTimestampToSeconds → "ADD"

    U.AddTez     → "ADD"
    U.SubTez     → "SUB"
    U.MulTezNat  → "MUL"
    U.MulNatTez  → "MUL"
    U.EdivTezNat → "DIV"
    U.EdivTez    → "DIV"

    U.Or         → "OR"
    U.And        → "AND"
    U.Xor        → "XOR"
    U.Not        → "NOT"

emitFinal ∷ ∀ a b . (R.Typeable a, R.Typeable b) ⇒ Expr a b → Text
emitFinal expr =
  let code =
        case expr of
          Nop → ""
          _   → emit expr in
  T.concat ["{", code, "}"]

emitType ∷ U.Type → T.Text
emitType U.UnitT         = "unit"
emitType U.KeyT          = "key"
emitType U.HashT         = "hash"
emitType U.IntT          = "int"
emitType U.TezT          = "tez"
emitType U.BoolT         = "bool"
emitType U.StringT       = "string"
emitType U.OperationT    = "operation"
emitType (U.EitherT a b) = T.concat ["(or ", emitType a, " ", emitType b, ")"]
emitType (U.PairT a b)   = T.concat ["(pair ", emitType a, " ", emitType b, ")"]
emitType (U.OptionT a)   = T.concat ["(option ", emitType a, ")"]
emitType (U.ListT a)     = T.concat ["(list ", emitType a, ")"]

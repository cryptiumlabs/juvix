-- | Gives the default execution environment for netToAst
-- Can be added to via core translation
module Juvix.Bohm.Default where

import           Juvix.Bohm.Shared
import qualified Juvix.Bohm.Type   as BT
import           Juvix.Library     hiding (empty, link)

import qualified Data.Map.Strict   as Map
import           Text.Parsec.Expr

onIntGen ∷ (Int → Int → a) → Primitive → Primitive → Maybe a
onIntGen f (PInt i1) (PInt i2) = Just (f i1 i2)
onIntGen _ (PBool _) _         = Nothing
onIntGen _ _ (PBool _)         = Nothing

onInt ∷ (Int → Int → Int) → Primitive → Primitive → Maybe Primitive
onInt f = onIntGen (\x y -> PInt (f x y))

onIntB ∷ (Int → Int → Bool) → Primitive → Primitive → Maybe Primitive
onIntB f = onIntGen (\x y -> PBool (f x y))

onBool ∷ (Bool → Bool → Bool) → Primitive → Primitive → Maybe Primitive
onBool f (PBool b1) (PBool b2) = Just (PBool $ f b1 b2)
onBool _ (PInt _) _            = Nothing
onBool _ _ (PInt _)            = Nothing

plus ∷ Primitive → Primitive → Maybe Primitive
plus = onInt (+)

times ∷ Primitive → Primitive → Maybe Primitive
times = onInt (*)

minus ∷ Primitive → Primitive → Maybe Primitive
minus = onInt (-)

lt ∷ Primitive → Primitive → Maybe Primitive
lt = onIntB (<)

le ∷ Primitive → Primitive → Maybe Primitive
le = onIntB (<=)

gt ∷ Primitive → Primitive → Maybe Primitive
gt = onIntB (>)

ge ∷ Primitive → Primitive → Maybe Primitive
ge = onIntB (>=)

mod' ∷ Primitive → Primitive → Maybe Primitive
mod' = onInt mod

eq ∷ Primitive → Primitive → Maybe Primitive
eq = onIntB (==)

neq ∷ Primitive → Primitive → Maybe Primitive
neq = onIntB (/=)

and' ∷ Primitive → Primitive → Maybe Primitive
and' = onBool (&&)

or' ∷ Primitive → Primitive → Maybe Primitive
or' = onBool (||)

defaultEnv ∷ Map Symbol BT.Fn
defaultEnv =
  Map.fromList
    [ (intern "plus", BT.Arg2 plus)
    , (intern "+", BT.Arg2 plus)
    , (intern "*", BT.Arg2 times)
    , (intern "-", BT.Arg2 minus)
    , (intern "<>", BT.Arg2 neq)
    , (intern "<", BT.Arg2 lt)
    , (intern ">", BT.Arg2 gt)
    , (intern "<=", BT.Arg2 le)
    , (intern ">=", BT.Arg2 ge)
    , (intern "==", BT.Arg2 eq)
    , (intern "and", BT.Arg2 and')
    , (intern "or", BT.Arg2 or')
    , (intern "mod", BT.Arg2 mod')
    ]

defaultSymbols ∷ [Precedence]
defaultSymbols =
  [ Precedence {level = 7, symbol = "*", assoc = AssocLeft}
  , Precedence {level = 7, symbol = "div", assoc = AssocLeft}
  , Precedence {level = 6, symbol = "+", assoc = AssocLeft}
  , Precedence {level = 6, symbol = "-", assoc = AssocLeft}
  , Precedence {level = 4, symbol = "<>", assoc = AssocNone}
  , Precedence {level = 4, symbol = "<", assoc = AssocNone}
  , Precedence {level = 4, symbol = ">", assoc = AssocNone}
  , Precedence {level = 4, symbol = ">=", assoc = AssocNone}
  , Precedence {level = 4, symbol = "<=", assoc = AssocNone}
  , Precedence {level = 4, symbol = "==", assoc = AssocNone}
  , Precedence {level = 3, symbol = "and", assoc = AssocRight}
  , Precedence {level = 2, symbol = "or", assoc = AssocRight}
  , Precedence {level = 2, symbol = "mod", assoc = AssocRight}
  ]

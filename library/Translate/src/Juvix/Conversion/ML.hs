{-# OPTIONS_GHC -Wno-unused-imports #-}

module Juvix.Conversion.ML
  ( op,
  )
where

-- Target ML Syntax

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Desugar as Desugar
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Target
import Juvix.Library hiding (product, sum)
import qualified Juvix.Library.NameSymbol as NameSym
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

-- :defsig-match
-- type
-- declaration
-- open
op :: Sexp.T -> Target.TopLevel
op x
  | Sexp.isAtomNamed x ":type-class" = Target.TypeClass
  | Sexp.isAtomNamed x ":instance" = Target.TypeClassInstance
op (name Sexp.:> form)
  | Sexp.isAtomNamed name ":defsig-match" = Target.Function (defunSig form)
  | Sexp.isAtomNamed name "declare" = Target.Declaration (declaration form)
  | Sexp.isAtomNamed name "type" = Target.Type (type' form)
op _ = undefined

expression :: Sexp.T -> Target.Expression
expression = undefined

----------------------------------------------------------------------
-- Top Level Transformations
----------------------------------------------------------------------

defunSig :: Sexp.T -> Target.Function
defunSig = undefined

----------------------------------------
-- Declarations
----------------------------------------

declaration :: Sexp.T -> Target.Declaration
declaration = Target.Infixivity . infix'

infix' :: Sexp.T -> Target.InfixDeclar
infix' (Sexp.List [inf, n, i])
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    if  | Sexp.isAtomNamed inf "infix" ->
          Target.NonAssoc atomName (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixl" ->
          Target.AssocL atomName (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixr" ->
          Target.AssocR atomName (fromInteger atomNum)
        | otherwise -> error "malformed declaration"
infix' _ = error "malformed declaration"

----------------------------------------
-- Types
----------------------------------------
type' :: Sexp.T -> Target.Type
type' (assocName Sexp.:> args Sexp.:> dat)
  | Just symbolList <- toSymbolList args =
    Target.Typ usage symName symbolList (adtF sig (adt dat))
  where
    Assoc {symName, usage, sig} = handleAssocTypeName assocName
    adtF = maybe Target.NonArrowed Target.Arrowed
type' _ = error "malformed type declaration"

adt :: Sexp.T -> Target.Adt
adt (Sexp.List [rec']) =
  Target.Product (product rec')
adt sums =
  Target.Sum (sum sums)

sum :: Sexp.T -> NonEmpty Target.Sum
sum xs = Sexp.foldr f (pure (trans (Sexp.last xs))) (Sexp.butLast xs)
  where
    trans (name Sexp.:> contents)
      | Just n <- eleToSymbol name =
        case contents of
          Sexp.Nil ->
            Target.S n Nothing
          i ->
            Target.S n (Just (product i))
    trans _ = error "malformed sum"
    f ele =
      NonEmpty.cons (trans ele)

product :: Sexp.T -> Target.Product
product (Sexp.List [f@(name Sexp.:> form)])
  | Sexp.isAtomNamed name ":record-d"
      || Sexp.isAtomNamed name ":" =
    Target.Record (record f)
  | Sexp.isAtomNamed name ":arrow" =
    Target.Arrow (expression form)
product normal
  | Just list <- Sexp.toList normal =
    Target.ADTLike (expression <$> list)
product _ = error "malformed product"

record :: Sexp.T -> Target.Record
record (Sexp.List [name, form, sig])
  | Sexp.isAtomNamed name ":" =
    Target.Record'' (NonEmpty.fromList (recorDHelp form)) (Just (expression sig))
record form' =
  Target.Record'' (NonEmpty.fromList (recorDHelp form)) Nothing
  where
    form = Sexp.cdr form'

recorDHelp :: Sexp.T -> [Target.NameType]
recorDHelp =
  fmap f . groupBy2
  where
    f (n, sig) = Target.NameType' (expression sig) (name n)

name :: Sexp.T -> Target.Name
name (Sexp.List [n, a])
  | Sexp.isAtomNamed n ":implicit",
    Just atom <- eleToSymbol a =
    Target.Implicit atom
name a
  | Just atom <- eleToSymbol a = Target.Concrete atom
name _ = error "malformed record-name-field"

------------------------------
-- Type' Helper
------------------------------
data AssocTypeName
  = Assoc
      { symName :: Symbol,
        usage :: Maybe Target.Expression,
        sig :: Maybe Target.Expression
      }
  deriving (Show, Eq)

-- | @handleAssocTypeName@ takes a type declaration name and gives us
-- back a direct record of it's contents transformed into the target
-- syntax
handleAssocTypeName :: Sexp.T -> AssocTypeName
handleAssocTypeName (name Sexp.:> properties)
  | Just atomName <- eleToSymbol name =
    Assoc
      { symName = atomName,
        usage = fmap expression (assoc ":usage" group),
        sig = fmap expression (assoc ":type" group)
      }
  where
    group = groupBy2 properties
handleAssocTypeName name
  | Just Sexp.A {atomName} <- Sexp.atomFromT name =
    Assoc {symName = NameSym.toSymbol atomName, usage = Nothing, sig = Nothing}
  | otherwise = error "malformed type name"

--------------------------------------------------------------------------------
-- Misc Transformations
--------------------------------------------------------------------------------
----------------------------------------
-- Matching
----------------------------------------

arg :: Sexp.T -> Target.Arg
arg = undefined

--------------------------------------------------------------------------------
-- Expression Transformation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSym.toSymbol atomName)
  | otherwise = Nothing

toSymbolList :: Sexp.T -> Maybe [Symbol]
toSymbolList x = Sexp.toList x >>= traverse eleToSymbol

--------------------------------------------------------------------------------
-- Move to Sexp library
--------------------------------------------------------------------------------

assoc :: Foldable t => NameSym.T -> t (Sexp.T, b) -> Maybe b
assoc tag =
  fmap snd . find (flip Sexp.isAtomNamed tag . fst)

groupBy2 :: Sexp.T -> [(Sexp.T, Sexp.T)]
groupBy2 (a1 Sexp.:> a2 Sexp.:> rest) =
  (a1, a2) : groupBy2 rest
groupBy2 _ = []

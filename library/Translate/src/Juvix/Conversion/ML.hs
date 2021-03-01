{-# OPTIONS_GHC -Wno-unused-imports #-}

module Juvix.Conversion.ML
  ( op,
  )
where

import qualified Juvix.Desugar as Desugar
-- Target ML Syntax
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Target
import Juvix.Library
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
    Just Sexp.A {atomName} <- Sexp.atomFromT n =
    if  | Sexp.isAtomNamed inf "infix" ->
          Target.NonAssoc (NameSym.toSymbol atomName) (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixl" ->
          Target.AssocL (NameSym.toSymbol atomName) (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixr" ->
          Target.AssocR (NameSym.toSymbol atomName) (fromInteger atomNum)
        | otherwise -> error "malformed declaration"
infix' _ = error "malformed declaration"

----------------------------------------
-- Types
----------------------------------------
type' :: Sexp.T -> Target.Type
type' (name Sexp.:> args Sexp.:> dat) = undefined
  where
    assoc = handleAssocTypeName name

data AssocTypeName
  = Assoc
      { name :: Symbol,
        usage :: Maybe Target.Expression,
        sig :: Maybe Target.Expression
      }
  deriving (Show, Eq)

handleAssocTypeName :: Sexp.T -> AssocTypeName
handleAssocTypeName (name Sexp.:> properties)
  | Just Sexp.A {atomName} <- Sexp.atomFromT name =
    Assoc
      { name = NameSym.toSymbol atomName,
        usage = fmap expression (assoc ":usage" group),
        sig = fmap expression (assoc ":type" group)
      }
  where
    group = groupBy2 properties
handleAssocTypeName name
  | Just Sexp.A {atomName} <- Sexp.atomFromT name =
    Assoc {name = NameSym.toSymbol atomName, usage = Nothing, sig = Nothing}
  | otherwise = error "malformed type name"

----------------------------------------------------------------------
-- Expression Transformation
----------------------------------------------------------------------

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

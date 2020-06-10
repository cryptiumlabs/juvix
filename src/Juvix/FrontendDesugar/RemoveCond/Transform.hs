module Juvix.FrontendDesugar.RemoveCond.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.FrontendDesugar.RemoveCond.Types as New
import qualified Juvix.FrontendDesugar.RemoveGuard.Types as Old
import Juvix.Library

-- The actual transform we are doing
transformCond :: Old.Cond Old.Expression -> New.Expression
transformCond (Old.C xs) =
  foldr f (fList last []) xs
  where
    fList (Old.CondExpression pred body) falses =
      boolean "True" (transformExpression body)
        |> (NonEmpty.:| falses)
        |> New.Match'' (transformExpression pred)
        |> New.Match
    f conds falseBranch =
      fList conds [boolean "False" falseBranch]
    last =
      NonEmpty.last xs
    boolean symb body =
      New.MatchCon (symb :| []) []
        |> flip New.MatchLogic Nothing
        |> flip New.MatchL body

--------------------------------------------------------------------------------
-- Boilerplate Transforms
--------------------------------------------------------------------------------
transformtTopLevel :: Old.TopLevel -> New.TopLevel
transformtTopLevel (Old.Type t) = New.Type (transformType t)
transformtTopLevel (Old.ModuleOpen t) = New.ModuleOpen (transformModuleOpen t)
transformtTopLevel (Old.Signature t) = New.Signature undefined
transformtTopLevel (Old.Function t) = New.Function undefined
transformtTopLevel Old.TypeClass = New.TypeClass
transformtTopLevel Old.TypeClassInstance = New.TypeClassInstance

transformExpression :: Old.Expression -> New.Expression
transformExpression = undefined

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

transformType :: Old.Type -> New.Type
transformType (Old.Typ usage name' args form) =
  New.Typ (transformExpression <$> usage) name' args (transformTypeSum form)

transformTypeSum :: Old.TypeSum -> New.TypeSum
transformTypeSum (Old.Alias a) = New.Alias (transformAlias a)
transformTypeSum (Old.Data da) = New.Data (transformData da)

transformAlias :: Old.Alias -> New.Alias
transformAlias (Old.AliasDec exp) =
  New.AliasDec (transformExpression exp)

--------------------------------------------------
-- Arrows
--------------------------------------------------

transformNamedType :: Old.NamedType -> New.NamedType
transformNamedType (Old.NamedType' name exp) =
  New.NamedType' (transformName name) (transformExpression exp)

transformTypeRefine :: Old.TypeRefine -> New.TypeRefine
transformTypeRefine (Old.TypeRefine name refine) =
  New.TypeRefine (transformExpression name) (transformExpression refine)


--------------------------------------------------
-- Types Misc
--------------------------------------------------

transformName :: Old.Name -> New.Name
transformName (Old.Implicit s) = New.Implicit s
transformName (Old.Concrete s) = New.Concrete s

transformArrowSymbol :: Old.ArrowSymbol -> New.ArrowSymbol
transformArrowSymbol (Old.ArrowUse usage) =
  New.ArrowUse usage
transformArrowSymbol (Old.ArrowExp e) =
  New.ArrowExp (transformExpression e)

transformUniverseExpression
  :: Old.UniverseExpression -> New.UniverseExpression
transformUniverseExpression (Old.UniverseExpression s) =
  New.UniverseExpression s

--------------------------------------------------
-- ADTs
--------------------------------------------------

transformData :: Old.Data -> New.Data
transformData (Old.Arrowed exp adt) =
  New.Arrowed (transformExpression exp) (transformAdt adt)
transformData (Old.NonArrowed adt) =
  New.NonArrowed (transformAdt adt)

transformAdt :: Old.Adt -> New.Adt
transformAdt (Old.Sum oldsu) = New.Sum (transformSum <$> oldsu)
transformAdt (Old.Product p) = New.Product (transformProduct p)

transformSum :: Old.Sum -> New.Sum
transformSum (Old.S sym prod) =
  New.S sym (transformProduct <$> prod)

transformProduct :: Old.Product -> New.Product
transformProduct (Old.Record rec') = New.Record (transformRecord rec')
transformProduct (Old.Arrow arrow) = New.Arrow (transformExpression arrow)
transformProduct (Old.ADTLike adt) = New.ADTLike (transformExpression <$> adt)


transformRecord :: Old.Record -> New.Record
transformRecord (Old.Record'' fields sig) =
  New.Record'' (transformNameType <$> fields) (transformExpression <$> sig)

transformNameType :: Old.NameType -> New.NameType
transformNameType (Old.NameType' sig name) =
  New.NameType' (transformExpression sig) (transformName name)

--------------------------------------------------------------------------------
-- Functions And Modules
--------------------------------------------------------------------------------

transformFunction :: Old.Function -> New.Function
transformFunction (Old.Func f) = New.Func (transformFunctionLike f)

transformFunctionLike
  :: Old.FunctionLike Old.Expression -> New.FunctionLike New.Expression
transformFunctionLike (Old.Like name args body) =
  New.Like name (transformArg <$> args) (transformExpression body)

transformModuleOpen :: Old.ModuleOpen -> New.ModuleOpen
transformModuleOpen (Old.Open mod) = New.Open mod

transformArg :: Old.Arg -> New.Arg
transformArg = undefined

-- |
-- - FreeVars is an algorithm that checks for free symbols in the AST.
-- - The =ExcludedSet= holds the symbols defined... These are needed
--   in case of a degenerate case like
--   #+BEGIN_SRC ocaml
--     let foo =
--       let type point = {x : int, y : int} in
--       let our-point  = {x = 3, y = 4} in
--       our-point.x + our-point.y
--   #+END_SRC
--   + here we need to dismiss =our-point.x= and =our-point.y=, just
--     filtering out =our-point= isn't enough! we have to check if the
--     first-part of the name has =our-point=, since everything shares
--     the same namespace
-- - TODO :: How do we handle this case?
--   #+BEGIN_SRC ocaml
--     mod Foo where
--
--     let foo (x :: xs) = x + TopLevel.Foo.foo xs
--     let foo []        = 0
--   #+END_SRC
--   + To Handle this, we need to unqualify the foo, and have the
--     module handle the symbol allocation
-- - NOTE :: we assume lets are recursive, if this changes the code
--   has to be updated to account for that'
-- - NOTE :: we assume in =nameifyAdt= which takes effect in the =\\=
--   call to =nameifyLetType=, that definitions of constructors before
--   this point can't be redefined
--   + This means that if we have ordered definitions, we'll silently
--     drop the calls to the old constructors.
--   + Thus, please redefine the logic there to support such modes
module Juvix.FrontendContextualise.InfixPrecedence.FreeVars where

import qualified Data.HashSet as Set
import qualified Juvix.Core.Common.NameSymbol as NameSymb
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as Type
import Juvix.Library hiding (Set)

(\\) :: (Eq a, Hashable a) => Set.HashSet a -> Set.HashSet a -> Set.HashSet a
(\\) = Set.difference

infixr 5 \\

type ExcludedSet = Set.HashSet Symbol

type Set = Set.HashSet NameSymb.T

op :: ExcludedSet -> Type.Expression -> Set
op s (Type.Tuple tuple) = nameifyTuple s tuple
op s (Type.List list'') = nameifyList s list''
op s (Type.Primitive t) = Set.empty
op s (Type.Constant _c) = Set.empty
op s (Type.Let letbind) = nameifyLet s letbind
op s (Type.LetType le') = nameifyLetType s le'
op s (Type.Match match) = _nameifyMatch s match
op s (Type.Lambda lamb) = _nameifyLambda s lamb
op s (Type.Block block) = _nameifyBlock s block
op s (Type.ExpRecord i) = _nameifyExpRecord s i
op s (Type.ArrowE arro) = _nameifyArrowExp s arro
op s (Type.RefinedE re) = _nameifyTypeRefine s re
op s (Type.Parened par) = op s par
op s (Type.DeclarationE e) = _nameifyDeclarationExpression s e
op s (Type.UniverseName i) = _nameifyUniverseExpression s i
op s (Type.NamedTypeE nem) = nameifyNamedType s nem
op s (Type.Application ap) = nameifyApplication s ap
op s (Type.Name varNam)
  | Set.member (NameSymb.hd varNam) s = Set.singleton varNam
  | otherwise = Set.empty

------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------

nameifyTuple s (Type.TupleLit t) = foldMap (op s) t

nameifyList s (Type.ListLit t) = foldMap (op s) t

nameifyBlocks s (Type.Bloc expr) = op s expr

nameifyExprRecord s (Type.ExpressionRecord flds) =
  foldMap (nameifyNameSet s op) flds

nameifyNameSet s nameify (Type.NonPunned _ e) =
  nameify s e

nameifyType s (Type.Typ usage name args form) =
  -- only insert the name into the set as the args come after the usage
  foldMap (op (Set.insert name s)) usage
    <> nameifyData newS form
    \\ newBinds
  where
    newS = Set.fromList (name : args)
    newBinds = Set.fromList (fmap NameSymb.fromSymbol (name : args))

nameifyData s (Type.Arrowed ex adt) = op s ex <> nameifyAdt s adt
nameifyData s (Type.NonArrowed adt) = nameifyAdt s adt

-- note we foldMap here, so previous binds can't really pass on here
-- so we must remove the constructors being used in other branches
nameifyAdt s (Type.Sum oldsu) = foldMap (nameifySum s) oldsu
nameifyAdt s (Type.Product p) = nameifyProduct s p

-- we don't update s, as we remove conName in nameifyLetType
-- update this code if the top comment invariant does change
nameifySum s (Type.S _conName prod) =
  foldMap (nameifyProduct s) prod

nameifyProduct s (Type.Record rec') = nameifyRecord s rec'
nameifyProduct s (Type.Arrow arrow) = op s arrow
nameifyProduct s (Type.ADTLike adt) = foldMap (op s) adt

nameifyRecord s (Type.Record'' fields sig) =
  foldMap (nameifyNameType s) fields <> foldMap (op s) sig

-- we don't care about the name as it's called with module syntax
-- thus we check the first part in the exclusion set
nameifyNameType s (Type.NameType' sig _) =
  op s sig

nameifyApplication s (Type.App fun args) =
  op s fun <> foldMap (op s) args

nameifyNamedType s (Type.NamedType' _name exp) =
  op s exp

-----------------------------------------------------------
-- Functions for finding extra bindings
------------------------------------------------------------

typeNames (Type.Typ _ name _ form) =
  [name] <> dataName form

dataName (Type.Arrowed __ adt) = adtName adt
dataName (Type.NonArrowed adt) = adtName adt

-- product just gives untagged records, which we ignore
-- as they are invocated by a .
adtName (Type.Sum oldsum) = foldMap sumName oldsum
adtName (Type.Product _p) = []

sumName (Type.S conName _prod) = [conName]

------------------------------------------------------------
-- Actual bindings
------------------------------------------------------------

nameifyLet s (Type.LetGroup name bindings body) =
  foldMap (nameifyFunctionLike newS) bindings
    |> (<> op newS body)
    -- TODO ∷
    -- Is this call even needed anymore, due to having a
    -- set where we filter out from the set?
    -- verify before deleting.
    |> Set.delete (NameSymb.fromSymbol name)
  where
    newS = Set.insert name s

-- the s here for nameifyType is correct
nameifyLetType s (Type.LetType'' typ body) =
  nameifyType s typ <> op newS body \\ definedNames
  where
    definedNamesList = typeNames typ
    newS =
      Set.fromList definedNamesList <> s
    -- TODO ∷ verify this works in a case where you call
    -- a previously defined constructor in a branch before you
    -- define it?
    --
    -- This would result in a redefinition, however if the language
    -- semantics are unordered then this is impossible
    --
    -- this is needed for the case where one constructor calls another
    -- in the same definition.
    definedNames =
      Set.fromList (fmap NameSymb.fromSymbol definedNamesList)

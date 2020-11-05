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
-- - NOTE :: we assume in =nameifyAdt= which takes effect in the =\\=
--   call to =nameifyLetType=, that definitions of constructors before
--   this point can't be redefined
--   + This means that if we have ordered definitions, we'll silently
--     drop the calls to the old constructors.
--   + Thus, please redefine the logic there to support such modes
-- - _Reasons to update_
--   1. let's not being recursive
--      - we assume lets are recursive, if this changes the code
--        has to be updated to account for that'
--
--   2. Language becomes ordered
--      - see first note above
--
--   3. Universe or Declaration talk about free variables
--      - currently universe is unfinished, and are not
--        first class
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
module Juvix.FrontendContextualise.InfixPrecedence.FreeVars where

import qualified Data.HashSet as Set
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as Type
import Juvix.Library hiding (Set)
import qualified Juvix.Library.NameSymbol as NameSymb

(\\) :: (Eq a, Hashable a) => Set.HashSet a -> Set.HashSet a -> Set.HashSet a
(\\) = Set.difference

infixr 5 \\

type ExcludedSet = Set.HashSet Symbol

type Set = Set.HashSet NameSymb.T

-- universe names not supported
op :: ExcludedSet -> Type.Expression -> Set
op s (Type.Tuple tuple) = nameifyTuple s tuple
op s (Type.List list'') = nameifyList s list''
op _ (Type.Primitive _) = Set.empty
op _ (Type.Constant _c) = Set.empty
op s (Type.Let letbind) = nameifyLet s letbind
op s (Type.LetType le') = nameifyLetType s le'
op s (Type.Match match) = nameifyMatch s match
op s (Type.Lambda lamb) = nameifyLambda s lamb
op s (Type.Block block) = nameifyBlock s block
op s (Type.ExpRecord i) = nameifyExpRecord s i
op s (Type.ArrowE arro) = nameifyArrowExp s arro
op s (Type.RefinedE re) = nameifyTypeRefine s re
op s (Type.Parened par) = op s par
op s (Type.DeclarationE e) = nameifyDeclarationExpression s e
op _ (Type.UniverseName _) = Set.empty
op s (Type.NamedTypeE nem) = nameifyNamedType s nem
op s (Type.Application ap) = nameifyApplication s ap
op s (Type.Name varNam)
  | Set.member (NameSymb.hd varNam) s = Set.singleton varNam
  | otherwise = Set.empty

------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------

nameifyTuple :: ExcludedSet -> Type.Tuple -> Set
nameifyTuple s (Type.TupleLit t) = foldMap (op s) t

nameifyList :: ExcludedSet -> Type.List -> Set
nameifyList s (Type.ListLit t) = foldMap (op s) t

nameifyBlock :: ExcludedSet -> Type.Block -> Set
nameifyBlock s (Type.Bloc expr) = op s expr

nameifyExpRecord :: ExcludedSet -> Type.ExpRecord -> Set
nameifyExpRecord s (Type.ExpressionRecord flds) =
  foldMap (nameifyNameSet s op) flds

nameifyNameSet :: excludset -> (excludset -> t -> set) -> Type.NameSet t -> set
nameifyNameSet s nameify (Type.NonPunned _ e) =
  nameify s e

nameifyType :: ExcludedSet -> Type.Type -> Set
nameifyType s (Type.Typ usage name args form) =
  -- only insert the name into the set as the args come after the usage
  foldMap (op (Set.insert name s)) usage
    <> nameifyData newS form
    \\ newBinds
  where
    newS = Set.fromList (name : args)
    newBinds = Set.fromList (fmap NameSymb.fromSymbol (name : args))

nameifyData :: ExcludedSet -> Type.Data -> Set
nameifyData s (Type.Arrowed ex adt) = op s ex <> nameifyAdt s adt
nameifyData s (Type.NonArrowed adt) = nameifyAdt s adt

-- note we foldMap here, so previous binds can't really pass on here
-- so we must remove the constructors being used in other branches
nameifyAdt :: ExcludedSet -> Type.Adt -> Set
nameifyAdt s (Type.Sum oldsu) = foldMap (nameifySum s) oldsu
nameifyAdt s (Type.Product p) = nameifyProduct s p

-- we don't update s, as we remove conName in nameifyLetType
-- update this code if the top comment invariant does change
nameifySum :: ExcludedSet -> Type.Sum -> Set
nameifySum s (Type.S _conName prod) =
  foldMap (nameifyProduct s) prod

nameifyProduct :: ExcludedSet -> Type.Product -> Set
nameifyProduct s (Type.Record rec') = nameifyRecord s rec'
nameifyProduct s (Type.Arrow arrow) = op s arrow
nameifyProduct s (Type.ADTLike adt) = foldMap (op s) adt

nameifyRecord :: ExcludedSet -> Type.Record -> Set
nameifyRecord s (Type.Record'' fields sig) =
  foldMap (nameifyNameType s) fields <> foldMap (op s) sig

-- we don't care about the name as it's called with module syntax
-- thus we check the first part in the exclusion set
nameifyNameType :: ExcludedSet -> Type.NameType -> Set
nameifyNameType s (Type.NameType' sig _) =
  op s sig

nameifyApplication :: ExcludedSet -> Type.Application -> Set
nameifyApplication s (Type.App fun args) =
  op s fun <> foldMap (op s) args

nameifyNamedType :: ExcludedSet -> Type.NamedType -> Set
nameifyNamedType s (Type.NamedType' _name exp) =
  op s exp

-- we currently don't care about declares
nameifyDeclarationExpression :: ExcludedSet -> Type.DeclarationExpression -> Set
nameifyDeclarationExpression s (Type.DeclareExpession _i e) =
  op s e

nameifyTypeRefine :: ExcludedSet -> Type.TypeRefine -> Set
nameifyTypeRefine s (Type.TypeRefine name refine) =
  op s name <> op s refine

nameifyArrowExp :: ExcludedSet -> Type.ArrowExp -> Set
nameifyArrowExp s (Type.Arr' left usage right) =
  foldMap (op s) [left, usage, right]

nameifyMatch :: ExcludedSet -> Type.Match -> Set
nameifyMatch s (Type.Match'' on bindings) =
  op s on <> foldMap (nameifyMatchL s) bindings

nameifyMatchL :: ExcludedSet -> Type.MatchL -> Set
nameifyMatchL s (Type.MatchL pat body) =
  op (Set.fromList binds <> s) body <> Set.fromList calls
  where
    (binds, calls) = collectNamesMatchLogic pat

nameifyLambda :: ExcludedSet -> Type.Lambda -> Set
nameifyLambda s (Type.Lamb args body) =
  op (Set.fromList binds <> s) body <> Set.fromList calls
  where
    (binds, calls) = foldMap collectNamesMatchLogic args

nameifyFunctionLike :: ExcludedSet -> Type.FunctionLike Type.Expression -> Set
nameifyFunctionLike s (Type.Like args body) =
  op (Set.fromList binds <> s) body <> Set.fromList calls
  where
    (binds, calls) = foldMap collectNamesArg args

-----------------------------------------------------------
-- Functions for finding extra bindings
------------------------------------------------------------

typeNames :: Type.Type -> [Symbol]
typeNames (Type.Typ _ name _ form) =
  [name] <> dataName form

dataName :: Type.Data -> [Symbol]
dataName (Type.Arrowed __ adt) = adtName adt
dataName (Type.NonArrowed adt) = adtName adt

-- product just gives untagged records, which we ignore
-- as they are invocated by a .
adtName :: Type.Adt -> [Symbol]
adtName (Type.Sum oldsum) = foldMap sumName oldsum
adtName (Type.Product _p) = []

sumName :: Type.Sum -> [Symbol]
sumName (Type.S conName _prod) = [conName]

------------------------------
-- for match
------------------------------

collectNamesMatchLogic :: Type.MatchLogic -> ([Symbol], [NameSymb.T])
collectNamesMatchLogic (Type.MatchLogic s name) =
  let (binds, calls) = collectNamesMatchLogicStart s
      name' = case name of
        Just n -> [n]
        Nothing -> []
   in (binds <> name', calls)

-- first argument is binds, second is calls
-- the constructor name doesn't get bound, but we do call it
collectNamesMatchLogicStart :: Type.MatchLogicStart -> ([Symbol], [NameSymb.T])
collectNamesMatchLogicStart (Type.MatchName sym) = ([sym], [])
collectNamesMatchLogicStart (Type.MatchConst __) = ([], [])
collectNamesMatchLogicStart (Type.MatchRecord r) = foldMap collectNamesNameSet r
collectNamesMatchLogicStart (Type.MatchCon _n l) = foldMap collectNamesMatchLogic l

collectNamesNameSet :: Type.NameSet Type.MatchLogic -> ([Symbol], [NameSymb.T])
collectNamesNameSet (Type.NonPunned _ e) =
  collectNamesMatchLogic e

collectNamesArg :: Type.Arg -> ([Symbol], [NameSymb.T])
collectNamesArg (Type.ConcreteA ml) = collectNamesMatchLogic ml
collectNamesArg (Type.ImplicitA ml) = collectNamesMatchLogic ml

------------------------------------------------------------
-- Actual bindings
------------------------------------------------------------

nameifyLet :: ExcludedSet -> Type.Let -> Set
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
nameifyLetType :: ExcludedSet -> Type.LetType -> Set
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

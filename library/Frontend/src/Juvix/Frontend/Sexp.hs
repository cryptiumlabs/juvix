module Juvix.Frontend.Sexp where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Frontend.Types as Types
import qualified Juvix.Frontend.Types.Base as Types
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

transTopLevel :: Types.TopLevel -> Sexp.T
transTopLevel (Types.ModuleOpen (Types.Open m)) =
  Sexp.list [Sexp.atom "open", Sexp.atom m]
transTopLevel Types.TypeClassInstance = Sexp.atom ":instance"
transTopLevel (Types.Declaration i) = Sexp.atom "declare" Sexp.:> transDeclaration i
transTopLevel (Types.Signature sig) = Sexp.atom ":defsig" Sexp.:> transSig sig
transTopLevel (Types.Function f) = transDefun f
transTopLevel (Types.Module m) = transModule m
transTopLevel Types.TypeClass = Sexp.atom ":type-class"
transTopLevel (Types.Type t) = transType t

transExpr :: Types.Expression -> Sexp.T
transExpr (Types.UniverseName n) = transUniverseExpression n
transExpr (Types.DeclarationE d) = transDeclarationExpression d
transExpr (Types.Application p) = transApplication p
transExpr (Types.NamedTypeE t) = transNamedType t
transExpr (Types.Primitive p) = transPrimitive p
transExpr (Types.ExpRecord r) = transExpRecord r
transExpr (Types.RefinedE i) = transTypeRefine i
transExpr (Types.Constant c) = transConstant c
transExpr (Types.OpenExpr o) = transOpen o
transExpr (Types.Parened e) = Sexp.list [Sexp.atom ":paren", transExpr e]
transExpr (Types.LetType l) = transLetType l
transExpr (Types.ModuleE m) = transModuleE m
transExpr (Types.ArrowE a) = transArrowE a
transExpr (Types.Lambda l) = transLambda l
transExpr (Types.Tuple t) = transTuple t
transExpr (Types.Match m) = transMatch m
transExpr (Types.Block b) = transBlock b
transExpr (Types.Infix i) = transInfix i
transExpr (Types.List t) = transList t
transExpr (Types.Cond c) = transCond transExpr c
transExpr (Types.Name n) = Sexp.atom n
transExpr (Types.Let l) = transLet l
transExpr (Types.Do d) = transDo d

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

transType :: Types.Type -> Sexp.T
transType typ =
  Sexp.listStar [Sexp.atom "type", name, args, dat]
  where
    (name, args, dat) = transTypeGen typ

transTypeGen :: Types.Type -> (Sexp.T, Sexp.T, Sexp.T)
transTypeGen (Types.Typ usage name args form) =
  (newName, Sexp.list (fmap fromSym args), newForm)
  where
    fromSym = Sexp.atom . NameSymbol.fromSymbol
    usageTrans expr =
      case usage of
        Nothing -> expr
        Just us -> Sexp.list [expr, Sexp.atom ":usage", transExpr us]
    detailedName =
      usageTrans (fromSym name)
    (newName, newForm) =
      case form of
        Types.Arrowed arr adt ->
          case detailedName of
            name Sexp.:> xs ->
              (Sexp.listStar [name, Sexp.atom ":type", transExpr arr, xs], transAdt adt)
            _ ->
              (Sexp.list [detailedName, Sexp.atom ":type", transExpr arr], transAdt adt)
        Types.NonArrowed adt ->
          (detailedName, transAdt adt)

transAdt :: Types.Adt -> Sexp.T
transAdt (Types.Sum sums) = Sexp.list (fmap transSum (NonEmpty.toList sums))
transAdt (Types.Product prod) = Sexp.list [transProduct prod]

transSum :: Types.Sum -> Sexp.T
transSum (Types.S cons Nothing) =
  Sexp.list [Sexp.atom (NameSymbol.fromSymbol cons)]
transSum (Types.S cons (Just t@(Types.ADTLike _))) =
  Sexp.listStar [Sexp.atom (NameSymbol.fromSymbol cons), transProduct t]
transSum (Types.S cons (Just t)) =
  Sexp.list [Sexp.atom (NameSymbol.fromSymbol cons), transProduct t]

transProduct :: Types.Product -> Sexp.T
transProduct (Types.ADTLike as) =
  Sexp.list (fmap transExpr as)
transProduct (Types.Arrow arr) = Sexp.list [Sexp.atom ":arrow", transExpr arr]
transProduct (Types.Record d) = transRecord d

transRecord :: Types.Record -> Sexp.T
transRecord (Types.Record'' fields sig) =
  sigFun (Sexp.listStar [Sexp.atom ":record-d", Sexp.list newName])
  where
    newName = NonEmpty.toList fields >>= f
      where
        f (Types.NameType' sig name) =
          [transName name, transExpr sig]
    sigFun expr =
      case sig of
        Nothing ->
          expr
        Just ex ->
          Sexp.list [Sexp.atom ":", expr, transExpr ex]

transName :: Types.Name -> Sexp.T
transName (Types.Implicit s) =
  Sexp.list [Sexp.atom ":implicit", Sexp.atom (NameSymbol.fromSymbol s)]
transName (Types.Concrete s) =
  Sexp.atom (NameSymbol.fromSymbol s)

--------------------------------------------------
-- Arrows
--------------------------------------------------

-- TODO ∷ remove form once we can get rid of all the pass cruft
-- this never happens!?
transNamedType :: Types.NamedType -> Sexp.T
transNamedType (Types.NamedType' _name exp) = transExpr exp

transTypeRefine :: Types.TypeRefine -> Sexp.T
transTypeRefine (Types.TypeRefine name refine) =
  Sexp.list [Sexp.atom ":refinement", transExpr name, transExpr refine]

--------------------------------------------------------------------------------
-- Function definition expansions
--------------------------------------------------------------------------------

transModule :: Types.Module -> Sexp.T
transModule (Types.Mod like) = listF body [Sexp.atom ":defmodule", name, args, body]
  where
    (name, args, body) =
      transLike True (Sexp.list . NonEmpty.toList . fmap transTopLevel) like
    listF (cond Sexp.:> _xs)
      | Sexp.atom ":cond" == cond =
        Sexp.list
    listF _ = Sexp.listStar

transDefun :: Types.Function -> Sexp.T
transDefun (Types.Func like) = Sexp.list [Sexp.atom ":defun", name, args, body]
  where
    (name, args, body) = transLike False transExpr like

transLetType :: Types.LetType -> Sexp.T
transLetType (Types.LetType'' bindings body) =
  Sexp.list [Sexp.atom ":let-type", name, args, dat, transExpr body]
  where
    (name, args, dat) = transTypeGen bindings

transLet :: Types.Let -> Sexp.T
transLet (Types.Let'' like rest) =
  Sexp.list
    [Sexp.atom "let", name, args, body, transExpr rest]
  where
    (name, args, body) = transLike False transExpr like

transModuleE :: Types.ModuleE -> Sexp.T
transModuleE (Types.ModE like rest) =
  Sexp.list [Sexp.atom ":let-mod", name, args, body, transExpr rest]
  where
    (name, args, body) =
      transLike True (Sexp.list . NonEmpty.toList . fmap transTopLevel) like

transLambda :: Types.Lambda -> Sexp.T
transLambda (Types.Lamb args expr) =
  Sexp.list
    [ Sexp.atom ":lambda",
      Sexp.list (NonEmpty.toList (fmap transMatchLogic args)),
      transExpr expr
    ]

transLike ::
  Bool -> (a -> Sexp.T) -> Types.FunctionLike' Types.T a -> (Sexp.T, Sexp.T, Sexp.T)
transLike many trans (Types.Like name args body) =
  (Sexp.atom (NameSymbol.fromSymbol name), Sexp.list (fmap transArg args), bTrans body)
  where
    bTrans = transGuardBody many trans

transGuardBody :: Bool -> (a -> Sexp.T) -> Types.GuardBody' Types.T a -> Sexp.T
transGuardBody _alsee trans (Types.Body b) = trans b
transGuardBody False trans (Types.Guard c) = transCond trans c
transGuardBody True transs (Types.Guard c) = transCondMultiple transs c

--------------------------------------------------------------------------------
-- Match Expansion
--------------------------------------------------------------------------------

transArg :: Types.Arg -> Sexp.T
transArg (Types.ImplicitA i) = Sexp.list [Sexp.atom ":implicit-a", transMatchLogic i]
transArg (Types.ConcreteA c) = transMatchLogic c

transMatch :: Types.Match -> Sexp.T
transMatch (Types.Match'' matchOn bindings) =
  Sexp.listStar [Sexp.atom "case", transExpr matchOn, binds]
  where
    binds = fmap transMatchL bindings |> NonEmpty.toList |> Sexp.list

transMatchL :: Types.MatchL -> Sexp.T
transMatchL (Types.MatchL pat body) =
  Sexp.list [transMatchLogic pat, transExpr body]

transMatchLogic :: Types.MatchLogic -> Sexp.T
transMatchLogic (Types.MatchLogic content (Just name)) =
  Sexp.list [Sexp.atom ":as", Sexp.atom name', transMatchStart content]
  where
    name' = NameSymbol.fromSymbol name
transMatchLogic (Types.MatchLogic content Nothing) =
  transMatchStart content

transMatchStart :: Types.MatchLogicStart -> Sexp.T
transMatchStart (Types.MatchName sym) = Sexp.atom (NameSymbol.fromSymbol sym)
transMatchStart (Types.MatchConst c) = transConstant c
transMatchStart (Types.MatchCon conName logics) =
  Sexp.listStar [Sexp.atom conName, Sexp.list (fmap transMatchLogic logics)]
transMatchStart (Types.MatchRecord fields) =
  Sexp.listStar [Sexp.atom ":record", recContents fields]
  where
    recContents =
      Sexp.list . NonEmpty.toList . fmap (transNameSet transMatchLogic)

transNameSet :: (t -> Sexp.T) -> Types.NameSet t -> Sexp.T
transNameSet _trans (Types.Punned t) =
  Sexp.list [Sexp.atom t]
transNameSet trans (Types.NonPunned t xs) =
  Sexp.list [Sexp.atom t, trans xs]

--------------------------------------------------------------------------------
-- Misc Expansion
--------------------------------------------------------------------------------

transDeclaration :: Types.Declaration -> Sexp.T
transDeclaration decl =
  case decl of
    Types.Infixivity (Types.AssocL n i) ->
      infixixgen "infixl" n i
    Types.Infixivity (Types.AssocR n i) ->
      infixixgen "infixr" n i
    Types.Infixivity (Types.NonAssoc n i) ->
      infixixgen "infix" n i
  where
    infixixgen name n i =
      Sexp.list
        [ Sexp.atom name,
          Sexp.atom (NameSymbol.fromSymbol n),
          Sexp.number (fromIntegral i)
        ]

transSig :: Types.Signature -> Sexp.T
transSig (Types.Sig name usage arrow constraints) =
  Sexp.list
    [ Sexp.atom (NameSymbol.fromSymbol name),
      usageTrans (constraintTrans (transExpr arrow))
    ]
  where
    usageTrans expr =
      case usage of
        Nothing -> expr
        Just us -> Sexp.list [Sexp.atom ":usage", transExpr us, expr]
    constraintTrans expr =
      case constraints of
        [] ->
          expr
        _ ->
          Sexp.list [Sexp.atom ":=>", Sexp.list (fmap transExpr constraints), expr]

transConstant :: Types.Constant -> Sexp.T
transConstant (Types.Number (Types.Integer' i)) = Sexp.number i
transConstant (Types.Number (Types.Double' _d)) = undefined
transConstant (Types.String (Types.Sho _t)) = undefined

transTuple :: Types.Tuple -> Sexp.T
transTuple (Types.TupleLit t) =
  Sexp.list (Sexp.atom ":tuple" : fmap transExpr t)

transList :: Types.List -> Sexp.T
transList (Types.ListLit t) =
  Sexp.list (Sexp.atom ":list" : fmap transExpr t)

transPrimitive :: Types.Primitive -> Sexp.T
transPrimitive (Types.Prim p) =
  Sexp.list [Sexp.atom ":primitive", Sexp.atom p]

-- TODO ∷ remove this repeat code
transCondMultiple :: (t -> Sexp.T) -> Types.Cond t -> Sexp.T
transCondMultiple trans (Types.C t) =
  Sexp.list (Sexp.atom ":cond" : NonEmpty.toList (fmap (transCondLogicMultiple trans) t))

transCondLogicMultiple :: (t -> Sexp.T) -> Types.CondLogic' Types.T t -> Sexp.T
transCondLogicMultiple trans (Types.CondExpression p b) =
  Sexp.listStar [transExpr p, trans b]

transCond :: (t -> Sexp.T) -> Types.Cond t -> Sexp.T
transCond trans (Types.C t) =
  Sexp.list (Sexp.atom ":cond" : NonEmpty.toList (fmap (transCondLogic trans) t))

transCondLogic :: (t -> Sexp.T) -> Types.CondLogic' Types.T t -> Sexp.T
transCondLogic trans (Types.CondExpression p b) =
  Sexp.list [transExpr p, trans b]

transOpen :: Types.ModuleOpenExpr -> Sexp.T
transOpen (Types.OpenExpress mod expr) =
  Sexp.list [Sexp.atom ":open-in", Sexp.atom mod, transExpr expr]

transApplication :: Types.Application -> Sexp.T
transApplication (Types.App expr args) =
  Sexp.listStar [transExpr expr, Sexp.list (NonEmpty.toList (fmap transExpr args))]

transBlock :: Types.Block -> Sexp.T
transBlock (Types.Bloc b) = Sexp.list [Sexp.atom ":progn", transExpr b]

transInfix :: Types.Infix -> Sexp.T
transInfix (Types.Inf l op r) =
  Sexp.list [Sexp.atom ":infix", Sexp.atom op, transExpr l, transExpr r]

transExpRecord :: Types.ExpRecord -> Sexp.T
transExpRecord (Types.ExpressionRecord fields) =
  Sexp.listStar [Sexp.atom ":record", recContents fields]
  where
    recContents =
      Sexp.list . NonEmpty.toList . fmap (transNameSet transExpr)

transDo :: Types.Do -> Sexp.T
transDo (Types.Do'' bs) =
  Sexp.listStar [Sexp.atom ":do", Sexp.list (NonEmpty.toList (fmap transDoBody bs))]

transDoBody :: Types.DoBody -> Sexp.T
transDoBody (Types.DoBody Nothing expr) =
  transExpr expr
transDoBody (Types.DoBody (Just n) expr) =
  Sexp.list [Sexp.atom "%<-", Sexp.atom (NameSymbol.fromSymbol n), transExpr expr]

transArrowE :: Types.ArrowExp -> Sexp.T
transArrowE (Types.Arr' l u r) =
  Sexp.list [Sexp.atom ":custom-arrow", transExpr u, transExpr l, transExpr r]

transUniverseExpression :: Types.UniverseExpression -> Sexp.T
transUniverseExpression (Types.UniverseExpression s) =
  Sexp.list [Sexp.atom ":u", Sexp.atom (NameSymbol.fromSymbol s)]

transDeclarationExpression :: Types.DeclarationExpression -> Sexp.T
transDeclarationExpression (Types.DeclareExpression d e) =
  Sexp.list [Sexp.atom ":declaim", transDeclaration d, transExpr e]

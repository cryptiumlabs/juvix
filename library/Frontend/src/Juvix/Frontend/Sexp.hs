module Juvix.Frontend.Sexp where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import qualified Juvix.Frontend.Types.Base as Types
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

transTopLevel :: Types.TopLevel -> Sexp.T
transTopLevel (Types.Type t) = undefined
transTopLevel (Types.ModuleOpen (Types.Open m)) =
  Sexp.list [Sexp.atom "open", Sexp.atom m]
transTopLevel (Types.Declaration i) =
  Sexp.listStar [Sexp.atom "declare", transDeclaration i]
transTopLevel (Types.Signature sig) =
  Sexp.listStar [Sexp.atom "defsig", transSig sig]
transTopLevel (Types.Function f) = transDefun f
transTopLevel (Types.Module m) = transModule m
transTopLevel Types.TypeClassInstance = Sexp.atom "instance"
transTopLevel Types.TypeClass = Sexp.atom "type-class"

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
        Just us -> Sexp.list [Sexp.atom "%usage", transExpr us, expr]
    constraintTrans expr =
      case constraints of
        [] ->
          expr
        _ ->
          Sexp.list [Sexp.atom "%=>", Sexp.list (fmap transExpr constraints), expr]

transExpr :: Types.Expression -> Sexp.T
transExpr (Types.Tuple t) = transTuple t
transExpr (Types.List t) = transList t
transExpr (Types.Primitive p) = transPrimitive p
transExpr (Types.Cond c) = transCond transExpr c
transExpr (Types.ModuleE m) = transModuleE m
transExpr (Types.Constant c) = transConstant c
transExpr (Types.Let l) = transLet l
transExpr (Types.Match m) = transMatch m
transExpr (Types.Name n) = Sexp.atom n
transExpr (Types.OpenExpr o) = transOpen o
transExpr (Types.Lambda l) = undefined

transTuple :: Types.Tuple -> Sexp.T
transTuple (Types.TupleLit t) =
  Sexp.list (Sexp.atom "%tuple" : fmap transExpr t)

transList :: Types.List -> Sexp.T
transList (Types.ListLit t) =
  Sexp.list (Sexp.atom "%list" : fmap transExpr t)

transPrimitive :: Types.Primitive -> Sexp.T
transPrimitive (Types.Prim p) =
  Sexp.list [Sexp.atom "%primitive", Sexp.atom p]

transCond :: (t -> Sexp.T) -> Types.Cond t -> Sexp.T
transCond trans (Types.C t) =
  Sexp.list (Sexp.atom "%cond" : NonEmpty.toList (fmap (transCondLogic trans) t))

transCondLogic :: (t -> Sexp.T) -> Types.CondLogic' Types.T t -> Sexp.T
transCondLogic trans (Types.CondExpression p b) =
  Sexp.list [transExpr p, trans b]

transOpen :: Types.ModuleOpenExpr -> Sexp.T
transOpen (Types.OpenExpress mod expr) =
  Sexp.list [Sexp.atom "%open-in", Sexp.atom mod, transExpr expr]

--------------------------------------------------------------------------------
-- Function definition expansions
--------------------------------------------------------------------------------

transModule :: Types.Module -> Sexp.T
transModule (Types.Mod like) = Sexp.list [Sexp.atom "%defmodule", name, args, body]
  where
    (name, args, body) =
      transLike (Sexp.list . NonEmpty.toList . fmap transTopLevel) like

transDefun :: Types.Function -> Sexp.T
transDefun (Types.Func like) = Sexp.list [Sexp.atom "%defun", name, args, body]
  where
    (name, args, body) = transLike transExpr like

transLet :: Types.Let -> Sexp.T
transLet (Types.Let'' like rest) =
  Sexp.list
    [Sexp.atom "let", name, Sexp.listStar [args, body], transExpr rest]
  where
    (name, args, body) = transLike transExpr like

transModuleE :: Types.ModuleE -> Sexp.T
transModuleE (Types.ModE like rest) =
  Sexp.list [Sexp.atom "%let-mod", name, Sexp.listStar [args, body], transExpr rest]
  where
    (name, args, body) =
      transLike (Sexp.list . NonEmpty.toList . fmap transTopLevel) like

transLike ::
  (a -> Sexp.T) -> Types.FunctionLike' Types.T a -> (Sexp.T, Sexp.T, Sexp.T)
transLike trans (Types.Like name args body) =
  (Sexp.atom (NameSymbol.fromSymbol name), Sexp.list (fmap transArg args), bTrans body)
  where
    bTrans = transGuardBody trans

transGuardBody :: (a -> Sexp.T) -> Types.GuardBody' Types.T a -> Sexp.T
transGuardBody trans (Types.Body b) = trans b
transGuardBody trans (Types.Guard c) = transCond trans c

--------------------------------------------------------------------------------
-- Match Expansion
--------------------------------------------------------------------------------

transArg :: Types.Arg -> Sexp.T
transArg (Types.ImplicitA i) = Sexp.list [Sexp.atom "%implicit", transMatchLogic i]
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
  Sexp.list [Sexp.atom "%as", Sexp.atom name', transMatchStart content]
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
  Sexp.listStar [Sexp.atom "%record", recContents fields]
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

transConstant :: Types.Constant -> Sexp.T
transConstant (Types.Number (Types.Integer' i)) = Sexp.number i
transConstant (Types.Number (Types.Double' _d)) = undefined
transConstant (Types.String (Types.Sho _t)) = undefined

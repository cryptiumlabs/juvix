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
transExpr (Types.ModuleE m) = undefined m

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

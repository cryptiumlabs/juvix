module Juvix.Contextify.Passes (resolveModule, inifixSoloPass) where

import Control.Lens hiding (op, (|>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Contextify.Environment as Env
import qualified Juvix.Contextify.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.Core.Common.Closure as Closure
import qualified Juvix.Core.Common.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Sexp.Structure as Structure
import Juvix.Sexp.Structure.Lens
import qualified StmContainers.Map as STM

resolveModule ::
  ExpressionIO m => Env.SexpContext -> m Env.SexpContext
resolveModule context =
  Env.passContextSingle context (\x -> x == ":atom" || x == Structure.nameOpenIn) openResolution

inifixSoloPass ::
  Expression m => Env.SexpContext -> m Env.SexpContext
inifixSoloPass context =
  Env.passContextSingle context (== Structure.nameInfix) infixConversion

type ExpressionIO m = (Env.ErrS m, Env.HasClosure m, MonadIO m)

type Expression m = (Env.ErrS m, Env.HasClosure m)

openResolution ::
  ExpressionIO m => Context.T term ty sumRep -> Sexp.Atom -> Sexp.T -> m Sexp.T
openResolution ctx a cdr
  | Just open <- Structure.toOpenIn (Sexp.Cons (Sexp.Atom a) cdr) =
    pure (open ^. body)
  | otherwise =
    atomResolution ctx a cdr

atomResolution ::
  ExpressionIO m => Context.T term ty sumRep -> Sexp.Atom -> Sexp.T -> m Sexp.T
atomResolution context atom@Sexp.A {atomName = name} sexpAtom = do
  closure <- ask @"closure"
  let symbolName = NameSymbol.hd name
  case Closure.lookup symbolName closure of
    Just Closure.Info {mOpen = Just prefix} ->
      -- we qualified it to a module which is already qualified
      pure (Sexp.addMetaToCar atom (Sexp.atom (prefix <> name)))
    Just Closure.Info {} -> pure sexpAtom
    Nothing -> do
      let qualified = context ^. Context._currentNameSpace . Context.qualifiedMap
      looked <- liftIO $ atomically $ STM.lookup symbolName qualified
      case looked of
        Just Context.SymInfo {mod = prefix} ->
          pure $ Sexp.addMetaToCar atom (Sexp.atom (prefix <> name))
        Nothing -> pure sexpAtom
atomResolution _ _ s = pure s

infixConversion ::
  (Env.ErrS m, Env.HasClosure m) => Context.T t y s -> Sexp.Atom -> Sexp.T -> m Sexp.T
infixConversion context atom list = do
  grouped <- groupInfix context (Sexp.Cons (Sexp.Atom atom) list)
  case Shunt.shunt grouped of
    Right shunted ->
      pure $ convertShunt shunted
    Left (Shunt.Clash pred1 pred2) ->
      throw @"error" (Env.Clash pred1 pred2)
    Left Shunt.MoreEles ->
      throw @"error" Env.ImpossibleMoreEles

------------------------------------------------------------
-- Helpers for infix conversion
------------------------------------------------------------

groupInfix ::
  (Env.ErrS m, Env.HasClosure m) =>
  Context.T t y s ->
  Sexp.T ->
  m (NonEmpty (Shunt.PredOrEle Sexp.T Sexp.T))
groupInfix context xs
  | Just inf <- Structure.toInfix xs,
    Just Sexp.A {atomName = opSym} <- Sexp.atomFromT (inf ^. op) = do
    prec <- Env.lookupPrecedence opSym context
    moreInfixs <- groupInfix context (inf ^. right)
    precedenceConversion (inf ^. op) prec
      |> Shunt.Precedence
      |> flip NonEmpty.cons moreInfixs
      -- we cons l and not r, as "3 + 4 + 5 * 6"
      -- parses as "3 + (4 + (5 * 6))"
      -- thus the left is always an element
      |> NonEmpty.cons (Shunt.Ele (inf ^. left))
      |> pure
  | otherwise =
    pure (Shunt.Ele xs :| [])

precedenceConversion ::
  Sexp.T -> Context.Precedence -> Shunt.Precedence Sexp.T
precedenceConversion s (Context.Pred Context.Left i) =
  Shunt.Pred s Shunt.Left' i
precedenceConversion s (Context.Pred Context.Right i) =
  Shunt.Pred s Shunt.Right' i
precedenceConversion s (Context.Pred Context.NonAssoc i) =
  Shunt.Pred s Shunt.NonAssoc i

convertShunt :: Shunt.Application Sexp.T Sexp.T -> Sexp.T
convertShunt (Shunt.Single e) = e
convertShunt (Shunt.App s app1 app2) =
  Sexp.list [s, convertShunt app1, convertShunt app2]

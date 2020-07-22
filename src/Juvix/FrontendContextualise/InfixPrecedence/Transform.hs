{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.InfixPrecedence.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Env
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as New
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Old
import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import qualified Data.Text as Text
import Juvix.Library


-- Pass we care about
-- This uses the shunt algorithm
transformInfix :: Env.WorkingMaps m => Old.Infix -> m New.Expression
transformInfix inf = do
  grouped <- groupInfixs (Old.Infix inf)
  case Shunt.shunt grouped of
    Right shunted ->
      transformExpression (convertOldApplication shunted)
    Left (Shunt.Clash pred1 pred2) ->
      throw @"error" (Env.Clash pred1 pred2)
    Left Shunt.MoreEles ->
      throw @"error" Env.ImpossibleMoreEles

----------------------------------------
-- Helpers for transformInfix
----------------------------------------
precedenceConversion :: Symbol -> Context.Precedence -> Shunt.Precedence
precedenceConversion s (Context.Pred Context.Left i) =
  Shunt.Pred s Shunt.Left' i
precedenceConversion s (Context.Pred Context.Right i) =
  Shunt.Pred s Shunt.Right' i
precedenceConversion s (Context.Pred Context.NonAssoc i) =
  Shunt.Pred s Shunt.NonAssoc i

groupInfixs ::
  Env.WorkingMaps m => Old.Expression -> m (NonEmpty (Shunt.PredOrEle Old.Expression))
groupInfixs (Old.Infix (Old.Inf l s r)) = do
  let reconstructedSymbol = reconstructSymbol s
  looked <- Env.lookup reconstructedSymbol
  case looked of
    Just Context.Def {precedence} ->
      let f xs =
            precedenceConversion reconstructedSymbol precedence
            |> Shunt.Precedence
            |> flip NonEmpty.cons xs
            |> NonEmpty.cons (Shunt.Ele l)
      in fmap f (groupInfixs r)
    _ -> throw @"error" (Env.UnknownSymbol reconstructedSymbol)
groupInfixs e = pure (Shunt.Ele e :| [])


convertOldApplication :: Shunt.Application Old.Expression -> Old.Expression
convertOldApplication (Shunt.Single e) =
  e
convertOldApplication (Shunt.App s app1 app2) =
  fmap convertOldApplication (app1 :| [app2])
  |> Old.App (Old.Name (deconstructSymbol s))
  |> Old.Application

-- TODO ∷ bad hack I'll have to change
reconstructSymbol :: NonEmpty Symbol -> Symbol
reconstructSymbol =
  intern . foldr (\x acc -> unintern x <> "." <> acc) mempty


deconstructSymbol :: Symbol -> NonEmpty Symbol
deconstructSymbol =
  NonEmpty.fromList . fmap internText . Text.split (== '.') . textify

transformExpression :: Env.WorkingMaps m => Old.Expression -> m New.Expression
transformExpression = undefined

module Juvix.Core.ErasedAnn.Conversion where

import Data.List ((\\))
import qualified Juvix.Core.Erased as Erased
import Juvix.Core.ErasedAnn.Types
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.Types as Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)

free :: forall primTy primVal. E.Term primTy primVal -> [Symbol]
free = Erased.free . E.eraseAnn

convertTerm :: forall primTy primVal. E.Term primTy primVal -> Usage.T -> AnnTerm primTy primVal
convertTerm term usage =
  let ty = E.getType term
      ty' = convertType ty
   in case term of
        E.Var sym _ -> Ann usage ty' (Var sym)
        E.Prim p _ -> Ann usage ty' (Prim p)
        E.Let sym bind' body' (bindTy', _) ->
          -- Calculate captures.
          let captures = Erased.free (Erased.Lam sym (E.eraseAnn body'))
              -- TODO: Is this the right usage?
              bind = convertTerm bind' usage
              body = convertTerm body' usage
              bindTy = convertType bindTy'
              -- TODO: Eventually add `let` to Michelson, probably, instead of this conversion.
              lamTy = Pi usage bindTy ty'
              lam = Ann usage lamTy (LamM captures [sym] body)
           in Ann usage ty' (AppM lam [bind])
        E.Lam sym body' _ ->
          -- TODO: Is this the right usage?
          let body = convertTerm body' usage
           in case body of
                -- Combine nested lambdas into multi-argument function.
                Ann _ _ (LamM cap' arg' body') ->
                  Ann usage ty' (LamM (cap' \\ [sym]) (sym : arg') body')
                _ ->
                  Ann usage ty' (LamM (free term) [sym] body)
        E.App f' a' _ ->
          let f = convertTerm f' usage
              a = convertTerm a' usage
           in case f of
                -- Combine nested application into multi-argument application.
                Ann _ _ (AppM f' a') ->
                  Ann usage ty' (AppM f' (a' <> [a]))
                _ ->
                  Ann usage ty' (AppM f [a])

convertType :: forall primTy primVal. E.Type primTy -> Type primTy primVal
convertType ty =
  case ty of
    E.Star u -> Star u
    E.SymT s -> SymT s
    E.PrimTy p -> PrimTy p
    E.Pi u a r ->
      let a' = convertType a
          r' = convertType r
       in Pi u a' r'

module Juvix.Core.ErasedAnn.Conversion where

import Data.List ((\\))
import qualified Juvix.Core.Erased as Erased
import Juvix.Core.ErasedAnn.Types
import qualified Juvix.Core.Erasure.Types as E
import Juvix.Library hiding (Type)
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Library.NameSymbol as NameSymbol

free :: forall primTy primVal. E.Term primTy primVal -> [NameSymbol.T]
free = Erased.free . E.eraseAnn

convertTerm :: E.Term primTy primVal -> Usage.T -> AnnTerm primTy primVal
convertTerm term usage =
  let ty = E.getType term
      ty' = convertType ty
   in case term of
        E.Var sym _ -> Ann usage ty' (Var sym)
        E.Prim p _ -> Ann usage ty' (Prim p)
        E.Let sym bind body (bindTy, _) ->
          -- Calculate captures.
          let captures = Erased.free (Erased.Lam sym (E.eraseAnn body))
              -- TODO: Is this the right usage?
              bind' = convertTerm bind usage
              body' = convertTerm body usage
              bindTy' = convertType bindTy
              -- TODO: Eventually add `let` to Michelson, probably, instead of this conversion.
              lamTy = Pi usage bindTy' ty'
              lam = Ann usage lamTy (LamM captures [sym] body')
           in Ann usage ty' $ AppM lam [bind']
        E.Lam sym body _ ->
          -- TODO: Is this the right usage?
          case convertTerm body usage of
            -- Combine nested lambdas into multi-argument function.
            Ann _ _ (LamM cap' arg' body') ->
              Ann usage ty' $ LamM (cap' \\ [sym]) (sym : arg') body'
            body' ->
              Ann usage ty' $ LamM (free term) [sym] body'
        E.Pair left right _ ->
          let left' = convertTerm left usage
              right' = convertTerm right usage
           in Ann usage ty' $ PairM left' right'
        E.App f a _ ->
          case convertTerm f usage of
            -- Combine nested application into multi-argument application.
            Ann _ _ (AppM f' as) ->
              Ann usage ty' $ AppM f' (as <> [a'])
            f' ->
              Ann usage ty' $ AppM f' [a']
          where
            a' = convertTerm a usage

convertType :: E.Type primTy -> Type primTy
convertType ty =
  case ty of
    E.Star u -> Star u
    E.SymT s -> SymT s
    E.PrimTy p -> PrimTy p
    E.Pi u a r -> Pi u (convertType a) (convertType r)
    E.Sig u a b -> Sig u (convertType a) (convertType b)

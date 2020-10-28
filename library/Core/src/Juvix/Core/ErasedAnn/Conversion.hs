module Juvix.Core.ErasedAnn.Conversion where

import Data.List ((\\))
import qualified Juvix.Core.Erased as Erased
import Juvix.Core.ErasedAnn.Types
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.Types as Types
import Juvix.Library hiding (Type)
import qualified Juvix.Library.Usage as Usage

free :: forall primTy primVal. E.Term primTy primVal -> [Symbol]
free = Erased.free . E.eraseAnn

convertTerm :: forall primTy primVal compErr m. (HasThrow "error" (Types.PipelineError primTy primVal compErr) m) => E.Term primTy primVal -> Usage.T -> m (AnnTerm primTy primVal)
convertTerm term usage = do
  let ty = E.getType term
  ty' <- convertType ty
  case term of
    E.Var sym _ -> pure (Ann usage ty' (Var sym))
    E.Prim p _ -> pure (Ann usage ty' (Prim p))
    E.Let sym bind body (bindTy, _) -> do
      -- Calculate captures.
      let captures = Erased.free (Erased.Lam sym (E.eraseAnn body))
      -- TODO: Is this the right usage?
      bind <- convertTerm bind usage
      body <- convertTerm body usage
      bindTy <- convertType bindTy
      -- TODO: Eventually add `let` to Michelson, probably, instead of this conversion.
      let lamTy = Pi usage bindTy ty'
          lam = Ann usage lamTy (LamM captures [sym] body)
      pure (Ann usage ty' (AppM lam [bind]))
    E.Lam sym body _ -> do
      -- TODO: Is this the right usage?
      body <- convertTerm body usage
      case body of
        -- Combine nested lambdas into multi-argument function.
        Ann _ _ (LamM cap' arg' body') ->
          pure (Ann usage ty' (LamM (cap' \\ [sym]) (sym : arg') body'))
        _ ->
          pure (Ann usage ty' (LamM (free term) [sym] body))
    E.Pair left right _ -> do
      left <- convertTerm left usage
      right <- convertTerm right usage
      pure (Ann usage ty' (PairM left right))
    E.App f a _ -> do
      f <- convertTerm f usage
      a <- convertTerm a usage
      case f of
        -- Combine nested application into multi-argument application.
        Ann _ _ (AppM f' a') ->
          pure (Ann usage ty' (AppM f' (a' <> [a])))
        _ ->
          pure (Ann usage ty' (AppM f [a]))

convertType :: forall primTy primVal compErr m. (HasThrow "error" (Types.PipelineError primTy primVal compErr) m) => E.Type primTy -> m (Type primTy primVal)
convertType ty =
  case ty of
    E.Star u -> pure (Star u)
    E.SymT s -> pure (SymT s)
    E.PrimTy p -> pure (PrimTy p)
    E.Pi u a r -> do
      a <- convertType a
      r <- convertType r
      pure (Pi u a r)
    E.Sig u a b -> Sig u <$> convertType a <*> convertType b

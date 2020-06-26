module Juvix.Core.ErasedAnn.Conversion where

import qualified Juvix.Core.Erased.Types as E
import Juvix.Core.ErasedAnn.Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)

convertTerm :: forall primTy primVal m. (Monad m) => E.Term primTy primVal -> Usage.T -> E.Term primTy primVal -> m (AnnTerm primTy primVal)
convertTerm term usage ty = do
  ty' <- convertType ty
  case term of
    E.Lam argUsage arg argType body -> do
      let E.Pi _ _ _ resTy = ty
      -- TODO captures
      body <- convertTerm body usage resTy
      pure (Ann usage ty' (LamM [] [arg] body))
    E.Elim elimUsage elim elimTy ->
      case elim of
        E.Var v ->
          pure (Ann usage ty' (Var v))
        E.Prim p ->
          pure (Ann usage ty' (Prim p))
        E.App fUsage f fTy xUsage x xTy -> do
          undefined
    -- TODO fixme T mismatches
    -- f <- convertTerm (E.Elim fUsage f fTy) fUsage fTy
    -- x <- convertTerm x xUsage xTy
    -- TODO: convert
    -- pure (Ann usage ty' (AppM f [x]))
    -- TODO let
    _ -> undefined

convertType :: forall primTy primVal m. (Monad m) => E.Term primTy primVal -> m (Type primTy primVal)
convertType term =
  case term of
    E.Elim _ elim _ ->
      case elim of
        E.Var s -> pure (SymT s)
    E.Star n -> pure (Star n)
    E.PrimTy p -> pure (PrimTy p)
    -- TODO: Deal with argument name.
    E.Pi argUsage arg argType res -> do
      res <- convertType res
      pure (Pi argUsage (SymT arg) res)

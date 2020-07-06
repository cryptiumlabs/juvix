module Juvix.Core.ErasedAnn.Conversion where

import Juvix.Core.ErasedAnn.Types
import qualified Juvix.Core.Erasure.Types as E
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type)

convertTerm :: forall primTy primVal m. (Monad m) => E.Term primTy primVal -> Usage.T -> m (AnnTerm primTy primVal)
convertTerm term usage = do
  let ty = E.getType term
  ty' <- convertType ty
  case term of
    E.Var sym _ -> pure (Ann usage ty' (Var sym))
    E.Prim p _ -> pure (Ann usage ty' (Prim p))
    E.Lam sym body _ -> do
      -- TODO: Is this the right usage?
      -- TODO: Deal with arguments, captures.
      body <- convertTerm body usage
      pure (Ann usage ty' (LamM [] [sym] body))
    E.App f a _ -> do
      f <- convertTerm f usage
      a <- convertTerm a usage
      -- TODO: Deal with multi-arg application.
      pure (Ann usage ty' (AppM f [a]))

convertType :: forall primTy primVal m. (Monad m) => E.Type primTy -> m (Type primTy primVal)
convertType ty =
  case ty of
    E.PrimTy p -> pure (PrimTy p)
    E.Pi u a r -> do
      a <- convertType a
      r <- convertType r
      pure (Pi u a r)

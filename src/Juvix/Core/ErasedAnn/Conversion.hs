module Juvix.Core.ErasedAnn.Conversion where

import qualified Juvix.Core.Erased.Types as E
import Juvix.Core.ErasedAnn.Types
import Juvix.Library hiding (Type)

convertTerm :: forall primTy primVal m . (Monad m) => E.Term primTy primVal -> m (Term primTy primVal)
convertTerm term =
  case term of
    E.Lam arg body -> do
      -- TODO captures
      body <- convertTerm body
      pure (LamM [] [arg] body)
    E.Elim elim ->
      case elim of
        E.Var v -> pure (Var v)
        E.Prim p -> pure (Prim p)
        E.App f x -> do
          f <- convertTerm f
          x <- convertTerm x
          -- TODO: convert
          pure (AppM f [x])
    -- TODO let
    _ -> undefined 

convertType :: forall primTy primVal m . (Monad m) => E.Term primTy primVal -> m (Type primTy primVal)
convertType term =
  case term of
    E.Var s -> pure (SymT s)
    E.Star n -> pure (Star n)
    E.PrimTy p -> pure (PrimTy p)
    -- TODO: Deal with argument name.
    E.Pi usage _ arg res -> pure (Pi usage arg res)

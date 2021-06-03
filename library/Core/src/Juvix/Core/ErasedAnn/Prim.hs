module Juvix.Core.ErasedAnn.Prim
  ( fromAnn,
    toAnn,
    fromPrimType,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn.Types as Types
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

type Take primTy primVal = App.Take (Types.Type primTy) primVal

fromAnn :: Types.AnnTerm primTy primVal -> Maybe (Take primTy primVal)
fromAnn (Types.Ann usage type' (Types.Prim p)) = Just $ App.Take usage type' p
fromAnn _ = Nothing

toAnn :: Take primTy primVal -> Types.AnnTerm primTy primVal
toAnn (App.Take usage type' term) = Types.Ann usage type' $ Types.Prim term

fromPrimType :: P.PrimType primTy -> Types.Type primTy
fromPrimType (P.PrimType tys) = go tys
  where
    go (t :| []) = Types.PrimTy t
    go (t :| (u : us)) = Types.Pi Usage.Omega (Types.PrimTy t) $ go $ u :| us

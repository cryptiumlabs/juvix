module Juvix.Core.ErasedAnn.Prim
  ( module Juvix.Core.ErasedAnn.Prim,

    -- * Constructors & fields for 'Return'
    pattern App.Cont,
    App.fun,
    App.args,
    App.numLeft,
    pattern App.Return,
    App.retType,
    App.retTerm,

    -- * Constructors & fields for 'Take'
    pattern App.Take,
    App.usage,
    App.type',
    App.term,
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
fromPrimType (t :| []) = Types.PrimTy t
fromPrimType (t :| (u : us)) =
  Types.Pi Usage.Omega (Types.PrimTy t) (fromPrimType (u :| us))

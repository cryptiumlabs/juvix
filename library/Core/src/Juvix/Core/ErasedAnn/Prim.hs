module Juvix.Core.ErasedAnn.Prim
  (module Juvix.Core.ErasedAnn.Prim,
   -- * Constructors & fields for 'Return'
   pattern App.Cont, App.fun, App.args, App.numLeft,
   pattern App.Return,
   -- * Constructors & fields for 'Take'
   pattern App.Take, App.usage, App.type', App.term,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn.Types as Types
import Juvix.Library

type Return primTy primVal = App.Return (Types.Type primTy primVal) primVal

type Take primTy primVal = App.Take (Types.Type primTy primVal) primVal


fromAnn :: Types.AnnTerm primTy primVal -> Maybe (Take primTy primVal)
fromAnn (Types.Ann usage type' (Types.Prim p)) = Just $ App.Take usage type' p
fromAnn _ = Nothing

toAnn :: Take primTy primVal -> Types.AnnTerm primTy primVal
toAnn (App.Take usage type' term) = Types.Ann usage type' $ Types.Prim term

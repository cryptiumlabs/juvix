module Juvix.Core.HR.Types where

import qualified Extensible as Ext
import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library


data T

IR.extendTerm "Term" [t|T|] $ IR.defaultExtTerm {
  IR.nameLam = "Lam0",
  IR.typeLam = Ext.Ann $ \_primTy _primVal -> [t|Symbol|]
  -- IR.typePi  = Ext.Ann $ \_primTy _primVal -> [t|Symbol|],
}

-- TODO allow extendTerm to reorder fields?
pattern Lam n b = Lam0 b n
{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}

IR.extendElim "Elim" [t|T|] $ IR.defaultExtElim {
  IR.typeBound = Ext.Disabled,
  IR.typeFree = Ext.Disabled,
  IR.typeElimX = [("Var", \_primTy _primVal -> [t|Symbol|])]
}

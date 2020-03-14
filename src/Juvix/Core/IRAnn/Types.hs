module Juvix.Core.IRAnn.Types where

import Juvix.Library
import qualified Extensible as Ex
import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Usage as Usage


data T

IR.extendTerm "Term" [t|T|] $ IR.defaultExtTerm
  { IR.typeLam = Ex.Ann $ \primTy primVal ->
      [t|(Usage.T, IR.Term' T $primTy $primVal)|]
  , IR.typeElim = Ex.Ann $ \primTy primVal ->
      [t|(Usage.T, IR.Term' T $primTy $primVal)|]
  }

IR.extendElim "Elim" [t|T|] $ IR.defaultExtElim
  { IR.typeApp = Ex.Ann $ \primTy primVal ->
      [t|(Usage.T, IR.Term' T $primTy $primVal,
          Usage.T, IR.Term' T $primTy $primVal)|]
  }

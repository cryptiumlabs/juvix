module Juvix.Core.HR.Extend where

import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library

extTerm =
  \_primTy _primVal ->
    IR.defaultExtTerm
      { IR.nameLam = "Lam0",
        IR.typeLam = Just [[t|Symbol|]],
        IR.namePi = "Pi0",
        IR.typePi = Just [[t|Symbol|]]
      }

extElim =
  \_primTy _primVal ->
    IR.defaultExtElim
      { IR.typeBound = Nothing,
        IR.typeFree = Nothing,
        IR.typeElimX = [("Var", [[t|Symbol|]])]
      }

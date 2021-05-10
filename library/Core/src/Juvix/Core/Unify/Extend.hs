module Juvix.Core.Unify.Extend (extTerm, extElim, extValue, extNeutral) where

import Extensible
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Core.Unify.MetaVar

extTerm :: TypeQ -> TypeQ -> IR.ExtTerm
extTerm _ty _val =
  IR.defaultExtTerm
    { IR.typeTermX = [("Meta", [[t|MetaVar|]])]
    }

extElim :: TypeQ -> TypeQ -> IR.ExtElim
extElim _ty _val = IR.defaultExtElim

extValue :: TypeQ -> TypeQ -> IR.ExtValue
extValue _ty _val =
  IR.defaultExtValue
    { IR.typeValueX = [("VMeta", [[t|MetaVar|]])]
    }

extNeutral :: TypeQ -> TypeQ -> IR.ExtNeutral
extNeutral _ty _val = IR.defaultExtNeutral

module Juvix.Core.HR.Extend where

import qualified Juvix.Core.Base.Types as Core
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | Extend binders, i.e Lam, Pi, Sig, Let with a human readable name (NameSymbol)
extTerm :: p1 -> p2 -> Core.ExtTerm
extTerm _primTy _primVal =
  Core.defaultExtTerm
    { Core.nameLam = "Lam0",
      Core.typeLam = Just [[t|NameSymbol.T|]],
      Core.namePi = "Pi0",
      Core.typePi = Just [[t|NameSymbol.T|]],
      Core.nameSig = "Sig0",
      Core.typeSig = Just [[t|NameSymbol.T|]],
      Core.nameLet = "Let0",
      Core.typeLet = Just [[t|NameSymbol.T|]]
    }

-- | Extend with extra constructor Var that was not existing before
extElim :: p1 -> p2 -> Core.ExtElim
extElim _primTy _primVal =
  Core.defaultExtElim
    { Core.typeBound = Nothing,
      Core.typeFree = Nothing,
      Core.typeElimX = [("Var", [[t|NameSymbol.T|]])]
    }

extPattern :: p1 -> p2 -> Core.ExtPattern
extPattern _primTy _primVal =
  Core.defaultExtPattern
    { Core.typePVar = Nothing,
      Core.typePatternX = [("PVar", [[t|NameSymbol.T|]])]
    }

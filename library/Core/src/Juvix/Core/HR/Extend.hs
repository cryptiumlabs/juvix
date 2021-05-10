module Juvix.Core.HR.Extend where

import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

extTerm :: p1 -> p2 -> IR.ExtTerm
extTerm =
  \_primTy _primVal ->
    IR.defaultExtTerm
      { IR.nameLam = "Lam0",
        IR.typeLam = Just [[t|NameSymbol.T|]],
        IR.namePi = "Pi0",
        IR.typePi = Just [[t|NameSymbol.T|]],
        IR.nameSig = "Sig0",
        IR.typeSig = Just [[t|NameSymbol.T|]],
        IR.nameLet = "Let0",
        IR.typeLet = Just [[t|NameSymbol.T|]]
      }

extElim :: p1 -> p2 -> IR.ExtElim
extElim =
  \_primTy _primVal ->
    IR.defaultExtElim
      { IR.typeBound = Nothing,
        IR.typeFree = Nothing,
        IR.typeElimX = [("Var", [[t|NameSymbol.T|]])]
      }

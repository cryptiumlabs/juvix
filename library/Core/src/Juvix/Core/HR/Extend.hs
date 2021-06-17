module Juvix.Core.HR.Extend where

import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | Extend binders (Lam, Pi, Sig, Let) of IR.Term with NameSymbol
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
        -- | Extend with extra constructor Var that was not existing before
        IR.typeElimX = [("Var", [[t|NameSymbol.T|]])]
      }

extPattern :: p1 -> p2 -> IR.ExtPattern
extPattern =
  \_primTy _primVal ->
    IR.defaultExtPattern
      { IR.typePVar = Nothing,
        IR.typePatternX = [("PVar", [[t|NameSymbol.T|]])]
      }

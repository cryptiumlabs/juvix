module Juvix.Core.HR.Types where

import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library

data T

IR.extendTerm "Term" [] [t|T|] $
  \_primTy _primVal ->
    IR.defaultExtTerm
      { IR.nameLam = "Lam0",
        IR.typeLam = Just [[t|Symbol|]],
        IR.namePi = "Pi0",
        IR.typePi = Just [[t|Symbol|]],
        IR.nameLet = "Let0",
        IR.typeLet = Just [[t|Symbol|]]
      }

-- TODO allow extendTerm to reorder fields?
pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

pattern Let π x l b = Let0 π l b x

{-# COMPLETE Star, PrimTy, Pi, Lam, Let, Elim #-}

IR.extendElim "Elim" [] [t|T|] $
  \_primTy _primVal ->
    IR.defaultExtElim
      { IR.typeBound = Nothing,
        IR.typeFree = Nothing,
        IR.typeElimX = [("Var", [[t|Symbol|]])]
      }

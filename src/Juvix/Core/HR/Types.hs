module Juvix.Core.HR.Types where

import Juvix.Library
import Juvix.Core.IR.Types.Base
import Juvix.Core.HR.Extend
import qualified Juvix.Core.IR.Types.Base as IR

data T

IR.extendTerm "Term" [] [t|T|] extTerm

-- TODO allow extendTerm to reorder fields?
pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}

IR.extendElim "Elim" [] [t|T|] extElim

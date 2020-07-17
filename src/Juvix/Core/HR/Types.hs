module Juvix.Core.HR.Types where

import Juvix.Core.HR.Extend
import Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library

data T

IR.extendTerm "Term" [] [t|T|] extTerm

-- TODO allow extendTerm to reorder fields?
pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

pattern Let π x l b = Let0 π l b x

{-# COMPLETE Star, PrimTy, Pi, Lam, Let, Elim #-}

IR.extendElim "Elim" [] [t|T|] extElim

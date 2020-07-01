module Juvix.Core.Erasure
  ( module Juvix.Core.Erasure.Algorithm,
    module Juvix.Core.Erasure.Types,
  )
where

import Juvix.Core.Erasure.Algorithm
import Juvix.Core.Erasure.Types
  (Error (..),
   Term,
   pattern Var,
   pattern Prim,
   pattern Lam,
   pattern Let,
   pattern App,
   Type,
   pattern SymT,
   pattern Star,
   pattern PrimTy,
   pattern Pi
  )

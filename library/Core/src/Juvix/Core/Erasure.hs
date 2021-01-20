module Juvix.Core.Erasure
  ( module Juvix.Core.Erasure.Algorithm,
    module Juvix.Core.Erasure.Types,
  )
where

import Juvix.Core.Erasure.Algorithm
import Juvix.Core.Erasure.Types
  ( Error (..),
    Globals,
    MapPrim,
    Term,
    TermT,
    Type,
    pattern App,
    pattern Lam,
    pattern Let,
    pattern Pi,
    pattern Prim,
    pattern PrimTy,
    pattern Star,
    pattern SymT,
    pattern Var,
  )

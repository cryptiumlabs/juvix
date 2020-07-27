module Juvix.Core.HRAnn.Types
  ( module Juvix.Core.HRAnn.Types,
    module Juvix.Core.HRAnn.Extend,
  )
where

import Juvix.Core.HRAnn.Extend
import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR

-- TODO: add combinators to @extensible-data@ for pairing like this
IR.extendTerm "Term" [] [t|T|] extTerm

-- TODO allow extendTerm to reorder fields?
pattern Lam π x s t = Lam0 t (BindAnnotation x (Annotation π s))

pattern Pi π x s t = Pi0 π s t x

pattern Let π x s l b = Let0 π l b (LetAnnotation x s)

pattern Elim π s t = Elim0 s (Annotation π t)

{-# COMPLETE Star, PrimTy, Pi, Lam, Let, Elim #-}

IR.extendElim "Elim" [] [t|T|] extElim

pattern App π s ts ρ t tt =
  App0 s t (AppAnnotation (Annotation π ts) (Annotation ρ tt))

{-# COMPLETE Var, Prim, App, Ann #-}

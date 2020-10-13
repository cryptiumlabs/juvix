module Juvix.Core.IRAnn.Types where

import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Usage as Usage
import Juvix.Library

data T

data Annotation primTy primVal = Annotation
  { usageAnn :: Usage.T,
    typeAnn :: IR.Term' T primTy primVal
  }

IR.extendTerm "Term" [] [t|T|] $
  \primTy primVal ->
    IR.defaultExtTerm
      { IR.nameLam = "Lam0",
        IR.typeLam = Just [[t|Annotation $primTy $primVal|]],
        IR.nameElim = "Elim0",
        IR.typeElim = Just [[t|Annotation $primTy $primVal|]]
      }

-- TODO allow extendTerm to reorder fields?
pattern Lam π s t = Lam0 t (Annotation π s)

pattern Elim π s t = Elim0 s (Annotation π t)

{-# COMPLETE Star, PrimTy, Prim, Pi, Lam, Elim #-}

data AppAnnotation primTy primVal = AppAnnotation
  { funAnn :: {-# UNPACK #-} !(Annotation primTy primVal),
    argAnn :: {-# UNPACK #-} !(Annotation primTy primVal)
  }

IR.extendElim "Elim" [] [t|T|] $
  \primTy primVal ->
    IR.defaultExtElim
      { IR.nameApp = "App0",
        IR.typeApp = Just [[t|AppAnnotation $primTy $primVal|]]
      }

pattern App π s ts ρ t tt =
  App0 s t (AppAnnotation (Annotation π ts) (Annotation ρ tt))

{-# COMPLETE Bound, Free, App, Ann #-}

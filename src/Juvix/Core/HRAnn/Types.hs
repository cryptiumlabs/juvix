module Juvix.Core.HRAnn.Types where

import qualified Extensible as Ext
import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Usage as Usage
import Juvix.Library

data T

data Annotation primTy primVal = Annotation
  { usageAnn :: Usage.T,
    typeAnn :: IR.Term' T primTy primVal
  }

data BindAnnotation primTy primVal = BindAnnotation
  { bindName :: Symbol,
    bindAnn :: {-# UNPACK #-} !(Annotation primTy primVal)
  }

-- TODO: add combinators to @extensible-data@ for pairing like this
IR.extendTerm "Term" [] [t|T|] $
  \primTy primVal ->
    IR.defaultExtTerm
      { IR.nameLam = "Lam0",
        IR.typeLam = Just [[t|BindAnnotation $primTy $primVal|]],
        IR.namePi = "Pi0",
        IR.typePi = Just [[t|Symbol|]],
        IR.nameElim = "Elim0",
        IR.typeElim = Just [[t|Annotation $primTy $primVal|]]
      }

-- TODO allow extendTerm to reorder fields?
pattern Lam π x s t = Lam0 t (BindAnnotation x (Annotation π s))

pattern Pi π x s t = Pi0 π s t x

pattern Elim π s t = Elim0 s (Annotation π t)

{-# COMPLETE Star, PrimTy, Pi, Lam, Elim #-}

data AppAnnotation primTy primVal = AppAnnotation
  { funAnn :: {-# UNPACK #-} !(Annotation primTy primVal),
    argAnn :: {-# UNPACK #-} !(Annotation primTy primVal)
  }

IR.extendElim "Elim" [] [t|T|] $
  \primTy primVal ->
    IR.defaultExtElim
      { IR.typeBound = Nothing,
        IR.typeFree = Nothing,
        IR.typeElimX = [("Var", [[t|Symbol|]])],
        IR.nameApp = "App0",
        IR.typeApp = Just [[t|AppAnnotation $primTy $primVal|]]
      }

pattern App π s ts ρ t tt =
  App0 s t (AppAnnotation (Annotation π ts) (Annotation ρ tt))

{-# COMPLETE Var, Prim, App, Ann #-}

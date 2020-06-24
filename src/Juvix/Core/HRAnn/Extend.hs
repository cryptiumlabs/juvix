module Juvix.Core.HRAnn.Extend where

import qualified Extensible as Ext
import qualified Juvix.Core.IR.Types.Base
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Usage as Usage
import Juvix.Library

data T

data Annotation primTy primVal
  = Annotation
      { usageAnn :: Usage.T,
        typeAnn :: IR.Term' T primTy primVal
      }

data BindAnnotation primTy primVal
  = BindAnnotation
      { bindName :: Symbol,
        bindAnn :: {-# UNPACK #-} !(Annotation primTy primVal)
      }

-- TODO: add combinators to @extensible-data@ for pairing like this
extTerm =
  \primTy primVal ->
    IR.defaultExtTerm
      { IR.nameLam = "Lam0",
        IR.typeLam = Just [[t|BindAnnotation $primTy $primVal|]],
        IR.namePi = "Pi0",
        IR.typePi = Just [[t|Symbol|]],
        IR.nameLet = "Let0",
        IR.typeLet = Just [[t|BindAnnotation $primTy $primVal|]],
        IR.nameElim = "Elim0",
        IR.typeElim = Just [[t|Annotation $primTy $primVal|]]
      }

data AppAnnotation primTy primVal
  = AppAnnotation
      { funAnn :: {-# UNPACK #-} !(Annotation primTy primVal),
        argAnn :: {-# UNPACK #-} !(Annotation primTy primVal)
      }

extElim =
  \primTy primVal ->
    IR.defaultExtElim
      { IR.typeBound = Nothing,
        IR.typeFree = Nothing,
        IR.typeElimX = [("Var", [[t|Symbol|]])],
        IR.nameApp = "App0",
        IR.typeApp = Just [[t|AppAnnotation $primTy $primVal|]]
      }

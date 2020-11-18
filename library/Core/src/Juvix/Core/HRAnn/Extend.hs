module Juvix.Core.HRAnn.Extend where

import qualified Extensible as Ext
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data T

data Annotation primTy primVal
  = Annotation
      { usageAnn :: Usage.T,
        typeAnn :: IR.Term' T primTy primVal
      }

data BindAnnotation primTy primVal
  = BindAnnotation
      { bindName :: NameSymbol.T,
        bindAnn :: {-# UNPACK #-} !(Annotation primTy primVal)
      }

data LetAnnotation primTy primVal
  = LetAnnotation
      { letName :: NameSymbol.T,
        letType :: IR.Term' T primTy primVal
      }

extTerm :: Ext.TypeQ -> Ext.TypeQ -> IR.ExtTerm
extTerm =
  \primTy primVal ->
    IR.defaultExtTerm
      { IR.nameLam = "Lam0",
        IR.typeLam = Just [[t|BindAnnotation $primTy $primVal|]],
        IR.namePi = "Pi0",
        IR.typePi = Just [[t|NameSymbol.T|]],
        IR.nameSig = "Sig0",
        IR.typeSig = Just [[t|NameSymbol.T|]],
        IR.nameLet = "Let0",
        IR.typeLet = Just [[t|LetAnnotation $primTy $primVal|]],
        IR.nameElim = "Elim0",
        IR.typeElim = Just [[t|Annotation $primTy $primVal|]]
      }

data AppAnnotation primTy primVal
  = AppAnnotation
      { funAnn :: {-# UNPACK #-} !(Annotation primTy primVal),
        argAnn :: {-# UNPACK #-} !(Annotation primTy primVal)
      }

extElim :: Ext.TypeQ -> Ext.TypeQ -> IR.ExtElim
extElim =
  \primTy primVal ->
    IR.defaultExtElim
      { IR.typeBound = Nothing,
        IR.typeFree = Nothing,
        IR.typeElimX = [("Var", [[t|NameSymbol.T|]])],
        IR.nameApp = "App0",
        IR.typeApp = Just [[t|AppAnnotation $primTy $primVal|]]
      }

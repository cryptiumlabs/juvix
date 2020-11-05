module Juvix.Core.IRAnn.Erasure where

import qualified Juvix.Core.IR.TransformExt as Trans
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IRAnn.Types as IRAnn

eraseTerm :: forall primTy primVal. IRAnn.Term primTy primVal -> IR.Term primTy primVal
eraseTerm = Trans.extForgetT

eraseElim :: forall primTy primVal. IRAnn.Elim primTy primVal -> IR.Elim primTy primVal
eraseElim = Trans.extForgetE

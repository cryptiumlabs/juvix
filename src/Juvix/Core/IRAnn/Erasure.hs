module Juvix.Core.IRAnn.Erasure where

import Juvix.Core.IR.TransformExt
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.IRAnn.Types

eraseTerm ∷ ∀ primTy primVal. Term primTy primVal → IR.Term primTy primVal
eraseTerm = extForgetT

eraseElim ∷ ∀ primTy primVal. Elim primTy primVal → IR.Elim primTy primVal
eraseElim = extForgetE

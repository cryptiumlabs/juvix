module Juvix.Core.Erased.Extend where

import Extensible (TypeQ)
import qualified Juvix.Core.HRAnn.Extend as HR
import Juvix.Core.IR.Types.Base

extTerm :: TypeQ -> TypeQ -> ExtTerm
extTerm = HR.extTerm

extElim :: TypeQ -> TypeQ -> ExtElim
extElim = HR.extElim

extValue :: p1 -> p2 -> ExtValue
extValue _ _ = defaultExtValue

extNeutral :: p1 -> p2 -> ExtNeutral
extNeutral _ _ = defaultExtNeutral

extPattern :: p1 -> p2 -> ExtPattern
extPattern _ _ = defaultExtPattern

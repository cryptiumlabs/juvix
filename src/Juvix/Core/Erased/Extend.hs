module Juvix.Core.Erased.Extend where

import qualified Juvix.Core.HRAnn.Extend as HR
import Juvix.Core.IR.Types.Base

-- extTerm :: template-haskell-2.15.0.0:Language.Haskell.TH.Lib.Internal.TypeQ
--                  -> template-haskell-2.15.0.0:Language.Haskell.TH.Lib.Internal.TypeQ
--                  -> ExtTerm
extTerm = HR.extTerm

-- extTerm :: template-haskell-2.15.0.0:Language.Haskell.TH.Lib.Internal.TypeQ
--                  -> template-haskell-2.15.0.0:Language.Haskell.TH.Lib.Internal.TypeQ
-- -> ExtElim
extElim = HR.extElim

extValue :: p1 -> p2 -> ExtValue
extValue = \_ _ -> defaultExtValue

extNeutral :: p1 -> p2 -> ExtNeutral
extNeutral = \_ _ -> defaultExtNeutral

extDatatype :: p1 -> p2 -> ExtDatatype
extDatatype = \_ _ -> defaultExtDatatype

extDataArg :: p1 -> p2 -> ExtDataArg
extDataArg = \_ _ -> defaultExtDataArg

extDataCon :: p1 -> p2 -> ExtDataCon
extDataCon = \_ _ -> defaultExtDataCon

extFunction :: p1 -> p2 -> ExtFunction
extFunction = \_ _ -> defaultExtFunction

extFunClause :: p1 -> p2 -> ExtFunClause
extFunClause = \_ _ -> defaultExtFunClause

extPattern :: p1 -> p2 -> ExtPattern
extPattern = \_ _ -> defaultExtPattern

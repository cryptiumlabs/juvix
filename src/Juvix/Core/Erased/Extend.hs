module Juvix.Core.Erased.Extend where

import qualified Juvix.Core.HR.Extend as HR
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.IR.Types.Base
import Juvix.Library

extTerm = \_primTy _primVal ->
  (HR.extTerm _primTy _primVal)
    { typeStar = Nothing
    }

extElim = HR.extElim

extDatatype = \_ _ -> defaultExtDatatype

extDataArg = \_ _ -> defaultExtDataArg

extDataCon = \_ _ -> defaultExtDataCon

extFunction = \_ _ -> defaultExtFunction

extFunClause = \_ _ -> defaultExtFunClause

extPattern = \_ _ -> defaultExtPattern

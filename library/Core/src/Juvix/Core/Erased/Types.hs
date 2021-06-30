module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term' (..),
    Type' (..),
    TypeAssignment',
  )
where

import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.Erased.Types.Base
import qualified Juvix.Core.IR.Typechecker.Types as TC

data T

extendTerm "Term" [] [t|T|] (\_ -> defaultExtTerm)

type TermT primTy primVal = Term (TC.TypedPrim primTy primVal)

extendType "Type" [] [t|T|] (\_ -> defaultExtType)

type Datatype = Core.Datatype' T T

type DataArg = Core.DataArg' T

type DataCon = Core.DataCon' T T

type Function = Core.Function' T

type FunClause primTy primVal = Core.FunClause' T primTy primVal

type TypeAssignment primTy = TypeAssignment' T primTy

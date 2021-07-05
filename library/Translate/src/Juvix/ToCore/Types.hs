{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Juvix.ToCore.Types (
  module Juvix.ToCore.Types,
  module Juvix.ToCore.Types.Defs,
  module Juvix.ToCore.Types.Env,
  module Juvix.ToCore.Types.Error
) where

import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Library hiding (show)
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Sexp as Sexp
import Juvix.Core.Translate ( hrToIR )
import Juvix.ToCore.Types.Env
import Juvix.ToCore.Types.Error
import Juvix.ToCore.Types.Defs

type ReduceEff ext primTy primVal m =
  ( HasThrowFF ext primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs ext primTy primVal m
  )

deriving instance Data LineNum.T

deriving instance Data Sexp.Atom

deriving instance Data Sexp.T

hrToIRSig :: CoreSig HR.T ty val -> CoreSig IR.T ty val
hrToIRSig d@DataSig { dataType }  = d { dataType = hrToIR dataType}
hrToIRSig c@ConSig{ conType } = c { conType = hrToIR <$> conType}
hrToIRSig v@ValSig {valType} = v { valType = hrToIR valType }
hrToIRSig (SpecialSig s) = SpecialSig s

hrToIRSigs :: CoreSigs HR.T ty val -> CoreSigs IR.T ty val
hrToIRSigs sigs = hrToIRSig <$> sigs

hrToIRDef :: CoreDef HR.T ty val -> CoreDef IR.T ty val
hrToIRDef (SpecialDef sym s) = SpecialDef sym s
hrToIRDef (CoreDef global) = CoreDef (hrToIRRawGlobal global)

hrToIRRawGlobal (Core.RawGDatatype d@Core.RawDatatype {Core.rawDataArgs, Core.rawDataCons, ..}) = notImplemented
hrToIRRawGlobal (Core.RawGDataCon d@Core.RawDataCon {Core.rawConType, ..})= notImplemented
hrToIRRawGlobal (Core.RawGFunction f@Core.RawFunction {})= notImplemented
hrToIRRawGlobal (Core.RawGAbstract a@Core.RawAbstract {}) = notImplemented

hrToIRState :: FFState HR.T ty val -> FFState IR.T ty val
hrToIRState FFState{ coreSigs, coreDefs, .. } = FFState { 
    coreSigs = hrToIRSigs coreSigs,
    coreDefs = fmap hrToIRDef coreDefs ,
     .. }


throwFF :: HasThrowFF ext primTy primVal m => Error ext primTy primVal -> m a
throwFF = throw @"fromFrontendError"

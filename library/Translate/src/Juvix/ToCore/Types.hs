{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Juvix.ToCore.Types (
  module Juvix.ToCore.Types,
  module Juvix.ToCore.Types.Defs,
  module Juvix.ToCore.Types.Env,
  module Juvix.ToCore.Types.Error
) where

import qualified Juvix.Core.Base.Types as Core
-- import Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Library hiding (show)
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Sexp as Sexp
import Juvix.Core.Translate ( hrToIR, hrPatternsToIR, hrToIRWith )
import Juvix.ToCore.Types.Env
import Juvix.ToCore.Types.Error
import Juvix.ToCore.Types.Defs
import qualified Data.HashMap.Strict as HM


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

hrToIRDef :: CoreDef HR.T ty val -> (HM.HashMap Core.GlobalName Core.PatternVar, CoreDef IR.T ty val)
hrToIRDef (SpecialDef sym s) = SpecialDef sym s
hrToIRDef (CoreDef global) = CoreDef (hrToIRRawGlobal global)

hrToIRDefs :: CoreDefs HR.T ty val -> CoreDefs IR.T ty val
hrToIRDefs defs = hrToIRDef <$> defs

hrToIRRawGlobal :: Core.RawGlobal' HR.T primTy primVal -> State (HM.HashMap Core.GlobalName Core.PatternVar) (Core.RawGlobal' IR.T primTy primVal)
hrToIRRawGlobal (Core.RawGDatatype d) = Core.RawGDatatype <$> hrToIRRawDatatype d
hrToIRRawGlobal (Core.RawGDataCon d)= Core.RawGDataCon  <$> hrToIRRawDataCon d
hrToIRRawGlobal (Core.RawGFunction f)= Core.RawGFunction <$> hrToIRRawFun f
hrToIRRawGlobal (Core.RawGAbstract Core.RawAbstract {rawAbsType, ..})
  =  pure $ Core.RawGAbstract Core.RawAbstract { rawAbsType = hrToIR rawAbsType, ..}

hrToIRRawDatatype :: Core.RawDatatype' HR.T primTy primVal -> State (HM.HashMap Core.GlobalName Core.PatternVar) (Core.RawDatatype' IR.T primTy primVal)
hrToIRRawDatatype Core.RawDatatype {rawDataArgs, rawDataCons, ..} = do
  cons <- traverse hrToIRRawDataCon rawDataCons
  pure Core.RawDatatype {
    rawDataArgs = hrToIRRawArg <$> rawDataArgs, 
    rawDataCons = cons,
    ..}

hrToIRRawArg :: Core.RawDataArg' HR.T primTy primVal -> Core.RawDataArg' IR.T primTy primVal
hrToIRRawArg Core.RawDataArg {rawArgType, ..} = Core.RawDataArg { rawArgType = hrToIR rawArgType, ..}

hrToIRRawDataCon :: Core.RawDataCon' HR.T primTy primVal -> State (HM.HashMap Core.GlobalName Core.PatternVar) (Core.RawDataCon' IR.T primTy primVal)
hrToIRRawDataCon Core.RawDataCon {rawConType, rawConDef, ..} = do
  mDef <- traverse hrToIRRawFun rawConDef
  pure $ Core.RawDataCon { 
  rawConType = hrToIR rawConType, 
  rawConDef = mDef,
  ..}

hrToIRRawFun :: Core.RawFunction' HR.T primTy primVal -> State (HM.HashMap Core.GlobalName Core.PatternVar) (Core.RawFunction' IR.T primTy primVal)
hrToIRRawFun Core.RawFunction {rawFunType, rawFunClauses, ..} = do
  fclauses <- traverse hrToIRRawFunClause rawFunClauses
  pure $ Core.RawFunction {
  rawFunType = hrToIR rawFunType,
  rawFunClauses = fclauses,
  ..}

hrToIRRawFunClause :: Core.RawFunClause' HR.T primTy primVal -> State (HM.HashMap Core.GlobalName Core.PatternVar) (Core.RawFunClause' IR.T primTy primVal)
hrToIRRawFunClause Core.RawFunClause {rawClauseTel, rawClausePats, rawClauseBody, ..}
  = do
  let (pats, pattsTable) = hrPatternsToIR rawClausePats
  -- TODO: How to avoid clashes?
  modify $ HM.union pattsTable
  pure Core.RawFunClause {
    rawClauseTel = hrToIRRawTeleEle <$> rawClauseTel ,
    rawClausePats = pats ,
    rawClauseBody =  hrToIRWith pattsTable  rawClauseBody,
    ..
    }

hrToIRRawTeleEle :: forall primTy primVal. Core.RawTeleEle' HR.T primTy primVal -> Core.RawTeleEle' IR.T primTy primVal
hrToIRRawTeleEle Core.RawTeleEle {rawTy, rawExtension,.. } 
  = Core.RawTeleEle { 
    rawTy = hrToIR rawTy, 
    rawExtension = notImplemented, -- TODO: TransformExt.Coerce rawExtension,
    ..}


hrToIRState :: FFState HR.T ty val -> FFState IR.T ty val
hrToIRState FFState{ coreSigs, coreDefs, .. } = FFState { 
    coreSigs = hrToIRSigs coreSigs,
    coreDefs = fmap hrToIRDef coreDefs ,
     .. }


throwFF :: HasThrowFF ext primTy primVal m => Error ext primTy primVal -> m a
throwFF = throw @"fromFrontendError"

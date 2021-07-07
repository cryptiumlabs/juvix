{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.ToCore.Types
  ( module Juvix.ToCore.Types,
    module Juvix.ToCore.Types.Defs,
    module Juvix.ToCore.Types.Env,
    module Juvix.ToCore.Types.Error,
  )
where

-- import Juvix.Core.Base.Types as Core

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Core.Translate (hrToIR)
import qualified Juvix.Core.Translate as Translate
import Juvix.Library hiding (show)
import qualified Juvix.Library.LineNum as LineNum
import qualified Juvix.Sexp as Sexp
import Juvix.ToCore.Types.Defs
import Juvix.ToCore.Types.Env
import Juvix.ToCore.Types.Error

type ReduceEff ext primTy primVal m =
  ( HasThrowFF ext primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs ext primTy primVal m
  )

deriving instance Data LineNum.T

deriving instance Data Sexp.Atom

deriving instance Data Sexp.T

-- TODO: Move this to Translate module

hrToIRSig :: CoreSig HR.T ty val -> CoreSig IR.T ty val
hrToIRSig d@DataSig {dataType} = d {dataType = hrToIR dataType}
hrToIRSig c@ConSig {conType} = c {conType = hrToIR <$> conType}
hrToIRSig v@ValSig {valType} = v {valType = hrToIR valType}
hrToIRSig (SpecialSig s) = SpecialSig s

hrToIRSigs :: CoreSigs HR.T ty val -> CoreSigs IR.T ty val
hrToIRSigs sigs = hrToIRSig <$> sigs

hrToIRDef :: CoreDef HR.T ty val -> Core.PatternMap Core.GlobalName -> (Core.PatternMap Core.GlobalName, CoreDef IR.T ty val)
hrToIRDef (SpecialDef sym s) pats = (pats, SpecialDef sym s)
hrToIRDef (CoreDef global) pats =
  let (def, env) = Translate.exec pats mempty (hrToIRRawGlobal global)
   in (Translate.patToSym env, CoreDef def)

hrToIRDefs :: CoreDefs HR.T ty val -> (Core.PatternMap Core.GlobalName, CoreDefs IR.T ty val)
hrToIRDefs = HM.foldlWithKey' f (mempty, mempty)
  where
    f (m, defs) globalName def =
      let (m', def') = hrToIRDef def m
       in (m', HM.insert globalName def' defs)

--hrToIRDef <$> defs

hrToIRRawGlobal :: Core.RawGlobal' HR.T primTy primVal -> Translate.M (Core.RawGlobal' IR.T primTy primVal)
hrToIRRawGlobal (Core.RawGDatatype d) = Core.RawGDatatype <$> hrToIRRawDatatype d
hrToIRRawGlobal (Core.RawGDataCon d) = Core.RawGDataCon <$> hrToIRRawDataCon d
hrToIRRawGlobal (Core.RawGFunction f) = Core.RawGFunction <$> hrToIRRawFun f
hrToIRRawGlobal (Core.RawGAbstract Core.RawAbstract {rawAbsType, ..}) =
  pure $ Core.RawGAbstract Core.RawAbstract {rawAbsType = hrToIR rawAbsType, ..}

hrToIRRawDatatype :: Core.RawDatatype' HR.T primTy primVal -> Translate.M (Core.RawDatatype' IR.T primTy primVal)
hrToIRRawDatatype Core.RawDatatype {rawDataArgs, rawDataCons, ..} = do
  cons <- traverse hrToIRRawDataCon rawDataCons
  pure
    Core.RawDatatype
      { rawDataArgs = hrToIRRawArg <$> rawDataArgs,
        rawDataCons = cons,
        ..
      }

hrToIRRawArg :: Core.RawDataArg' HR.T primTy primVal -> Core.RawDataArg' IR.T primTy primVal
hrToIRRawArg Core.RawDataArg {rawArgType, ..} = Core.RawDataArg {rawArgType = hrToIR rawArgType, ..}

hrToIRRawDataCon :: Core.RawDataCon' HR.T primTy primVal -> Translate.M (Core.RawDataCon' IR.T primTy primVal)
hrToIRRawDataCon Core.RawDataCon {rawConType, rawConDef, ..} = do
  mDef <- traverse hrToIRRawFun rawConDef
  pure $
    Core.RawDataCon
      { rawConType = hrToIR rawConType,
        rawConDef = mDef,
        ..
      }

hrToIRRawFun :: Core.RawFunction' HR.T primTy primVal -> Translate.M (Core.RawFunction' IR.T primTy primVal)
hrToIRRawFun Core.RawFunction {rawFunType, rawFunClauses, ..} = do
  fclauses <- traverse hrToIRRawFunClause rawFunClauses
  pure $
    Core.RawFunction
      { rawFunType = hrToIR rawFunType,
        rawFunClauses = fclauses,
        ..
      }

hrToIRRawFunClause :: Core.RawFunClause' HR.T primTy primVal -> Translate.M (Core.RawFunClause' IR.T primTy primVal)
hrToIRRawFunClause Core.RawFunClause {rawClauseTel, rawClausePats, rawClauseBody, ..} =
  do
    pats <- traverse Translate.hrPatternToIR' rawClausePats
    body <- Translate.hrToIR' rawClauseBody
    pure
      Core.RawFunClause
        { rawClauseTel = hrToIRRawTeleEle <$> rawClauseTel,
          rawClausePats = pats,
          rawClauseBody = body,
          ..
        }

hrToIRRawTeleEle :: forall primTy primVal. Core.RawTeleEle' HR.T primTy primVal -> Core.RawTeleEle' IR.T primTy primVal
hrToIRRawTeleEle Core.RawTeleEle {rawTy, rawExtension, ..} =
  Core.RawTeleEle
    { rawTy = hrToIR rawTy,
      rawExtension = notImplemented, -- TODO: TransformExt.Coerce rawExtension,
      ..
    }

hrToIRState :: FFState HR.T ty val -> FFState IR.T ty val
hrToIRState FFState {coreSigs, coreDefs, ..} =
  FFState
    { coreSigs = hrToIRSigs coreSigs,
      coreDefs = snd $ hrToIRDefs coreDefs,
      ..
    }

throwFF :: HasThrowFF ext primTy primVal m => Error ext primTy primVal -> m a
throwFF = throw @"fromFrontendError"

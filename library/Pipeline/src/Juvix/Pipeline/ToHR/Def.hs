{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Pipeline.ToHR.Def where
( CoreSig(..),
  -- Special(..),
  CoreSigs,
  CoreDefs,
  defName,
  mergeSigs
)
where

import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

---------------------
-- Core Definition --
---------------------
data CoreDef ext primTy primVal
  = CoreDef !(Core.RawGlobal' ext primTy primVal)
  | SpecialDef !NameSymbol.T !Special
  deriving (Generic)

deriving instance
  ( Show primTy,
    Show primVal,
    Core.TermAll Show ext primTy primVal,
    Core.ElimAll Show ext primTy primVal,
    Core.PatternAll Show ext primTy primVal
  ) =>
  Show (CoreDef ext primTy primVal)

deriving instance
  ( Eq primTy,
    Eq primVal,
    Core.TermAll Eq ext primTy primVal,
    Core.ElimAll Eq ext primTy primVal,
    Core.PatternAll Eq ext primTy primVal
  ) =>
  Eq (CoreDef ext primTy primVal)

deriving instance
  ( Data primTy,
    Data primVal,
    Data ext,
    Core.TermAll Data ext primTy primVal,
    Core.ElimAll Data ext primTy primVal,
    Core.PatternAll Data ext primTy primVal
  ) =>
  Data (CoreDef ext primTy primVal)

type CoreDefs ext primTy primVal = HashMap Core.GlobalName (CoreDef ext primTy primVal)

defName :: CoreDef ext primTy primVal -> NameSymbol.T
defName = \case
  CoreDef (Core.RawGDatatype Core.RawDatatype {rawDataName = x}) -> x
  CoreDef (Core.RawGDataCon Core.RawDataCon {rawConName = x}) -> x
  CoreDef (Core.RawGFunction Core.RawFunction {rawFunName = x}) -> x
  CoreDef (Core.RawGAbstract Core.RawAbstract {rawAbsName = x}) -> x
  SpecialDef x _ -> x

hrToIRDef :: CoreDef HR.T ty val -> Core.PatternMap Core.GlobalName -> (Core.PatternMap Core.GlobalName, CoreDef IR.T ty val)
hrToIRDef (SpecialDef sym s) pats = (pats, SpecialDef sym s)
hrToIRDef (CoreDef global) pats = Core.hrToIRDef global pats
)

-- hrToIRDefs :: CoreDefs HR.T ty val -> (Core.PatternMap Core.GlobalName, CoreDefs IR.T ty val)
-- hrToIRDefs = HM.foldlWithKey' f (mempty, mempty)
--   where
--     f (m, defs) globalName def =
--       let (m', def') = hrToIRDef def m
--        in (m', HM.insert globalName def' defs)

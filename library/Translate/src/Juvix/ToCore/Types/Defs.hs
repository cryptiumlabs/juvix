{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.ToCore.Types.Defs where

import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

---------------------
-- Core Signatures --
---------------------
data CoreSig ext primTy primVal
  = DataSig
      { dataType :: !(Core.Term' ext primTy primVal),
        dataCons :: [NameSymbol.T]
      }
  | ConSig
      { conType :: !(Maybe (Core.Term' ext primTy primVal))
      }
  | ValSig
      { valUsage :: !Core.GlobalUsage,
        valType :: !(Core.Term' ext primTy primVal)
      }
  | SpecialSig !Special
  deriving (Generic)

-- | Bindings that can't be given types, but can be given new names by the user.
data Special
  = -- | pi type, possibly with usage already supplied
    ArrowS (Maybe Usage.T)
  | -- | sigma type
    PairS (Maybe Usage.T)
  | -- | type annotation
    ColonS
  | -- | type of types
    TypeS
  | -- | omega usage
    OmegaS
  deriving (Eq, Show, Data, Generic)

deriving instance
  ( Eq primTy,
    Eq primVal,
    Core.TermAll Eq ext primTy primVal,
    Core.ElimAll Eq ext primTy primVal
  ) =>
  Eq (CoreSig ext primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Core.TermAll Show ext primTy primVal,
    Core.ElimAll Show ext primTy primVal
  ) =>
  Show (CoreSig ext primTy primVal)

deriving instance
  ( Data ext,
    Data primTy,
    Data primVal,
    Core.TermAll Data ext primTy primVal,
    Core.ElimAll Data ext primTy primVal
  ) =>
  Data (CoreSig ext primTy primVal)

type CoreSigs ext primTy primVal =
  HashMap Core.GlobalName (CoreSig ext primTy primVal)

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

-- | If two signatures can be merged (currently, only constructor signatures),
-- then do so, otherwise return the *first* unchanged
-- (since @insertWith@ calls it as @mergeSigs new old@).
mergeSigs ::
  CoreSig ext primTy primVal ->
  CoreSig ext primTy primVal ->
  CoreSig ext primTy primVal
mergeSigs (ConSig newTy) (ConSig oldTy) =
  ConSig (newTy <|> oldTy)
mergeSigs _ second = second

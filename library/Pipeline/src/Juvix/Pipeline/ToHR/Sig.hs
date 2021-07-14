{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Pipeline.ToHR.Sig where

import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

---------------------
-- Core Signatures --
---------------------
data CoreSig ext primTy primVal
  = CoreSig (Core.Sig ext primTy primVal)
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

hrToIRSig :: CoreSig HR.T ty val -> CoreSig IR.T ty val
hrToIRSig (CoreSig s)
hrToIRSig (SpecialSig s) = SpecialSig s

hrToIRSigs :: CoreSigs HR.T ty val -> CoreSigs IR.T ty val
hrToIRSigs sigs = hrToIRSig <$> sigs

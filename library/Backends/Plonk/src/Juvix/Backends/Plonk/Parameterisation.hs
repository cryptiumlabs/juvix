{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.Plonk.Parameterisation
  ( module Juvix.Backends.Plonk.Parameterisation,
    module Types,
  )
where

import Data.Field.Galois (GaloisField)
import Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (many, try)

plonk :: GaloisField f => P.Parameterisation (PrimTy f) (PrimVal f)
plonk =
  P.Parameterisation
    { hasType = notImplemented,
      builtinTypes = notImplemented,
      builtinValues = notImplemented,
      parseTy = notImplemented,
      parseVal = notImplemented,
      reservedNames = notImplemented,
      reservedOpNames = notImplemented,
      stringTy = notImplemented,
      stringVal = notImplemented,
      intTy = notImplemented,
      intVal = notImplemented,
      floatTy = \_ _ -> False, -- Michelson does not support floats
      floatVal = const Nothing
    }

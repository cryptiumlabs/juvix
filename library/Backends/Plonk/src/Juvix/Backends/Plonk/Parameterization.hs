{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Juvix.Backends.Plonk.Parameterization
  ( module Juvix.Backends.Plonk.Parameterization,
    module Types,
  )
where

import Data.Field.Galois (GaloisField)
import Juvix.Backends.Plonk.Types as Types
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (many, try)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol

check3Equal :: Eq a => NonEmpty a -> Bool
check3Equal (x :| [y, z])
  | x == y && x == z = True
  | otherwise = False
check3Equal (_ :| _) = False

check2Equal :: Eq a => NonEmpty a -> Bool
check2Equal (x :| [y])
  | x == y = True
  | otherwise = False
check2Equal (_ :| _) = False

isBool :: PrimTy f -> Bool
isBool PrimTy = True --  TODO: May change

checkFirst2AndLast :: Eq t => NonEmpty t -> (t -> Bool) -> Bool
checkFirst2AndLast (x :| [y, last]) check
  | check2Equal (x :| [y]) && check last = True
  | otherwise = False
checkFirst2AndLast (_ :| _) _ = False

hasType :: (GaloisField f, Eq (PrimTy f)) => PrimVal f -> Param.PrimType (PrimTy f) -> Bool
hasType (PConst _v) ty
  | length ty == 1 = True
  | otherwise = False
-- BinOps
hasType PAdd ty = check3Equal ty
hasType PSub ty = check3Equal ty
hasType PMul ty = check3Equal ty
hasType PDiv ty = check3Equal ty
hasType PExp ty = check3Equal ty
hasType PMod ty = check3Equal ty
hasType PAnd ty = check3Equal ty
hasType POr ty = check3Equal ty
hasType PXor ty = check3Equal ty
-- UnOps
hasType PDup ty = check2Equal ty
hasType PIsZero ty = check2Equal ty
hasType PNot ty = check2Equal ty
hasType PShL ty = check2Equal ty
hasType PShR ty = check2Equal ty
hasType PRotL ty = check2Equal ty
hasType PRotR ty = check2Equal ty
hasType PAssertEq ty = check2Equal ty
hasType PAssertIt ty = check2Equal ty
-- CompOps
hasType PGt ty = checkFirst2AndLast ty isBool
hasType PGte ty = checkFirst2AndLast ty isBool
hasType PLt ty = checkFirst2AndLast ty isBool
hasType PLte ty = checkFirst2AndLast ty isBool
hasType PEq ty = checkFirst2AndLast ty isBool

builtinTypes :: Param.Builtins (PrimTy f) -- TODO: Revisit this
builtinTypes = Map.fromList [(NameSymbol.fromSymbol "Plonk.f", PrimTy)]

builtinValues :: Param.Builtins (PrimVal f)
builtinValues =
  Map.fromList $
    first NameSymbol.fromSymbol
      <$> [("Plonk.add", PAdd)] -- TODO: Do the rest

plonk :: (GaloisField f, Eq (PrimTy f)) => Param.Parameterisation (PrimTy f) (PrimVal f)
plonk =
  Param.Parameterisation
    { hasType,
      builtinTypes,
      builtinValues,
      parseTy = \_ -> pure PrimTy,
      parseVal = notImplemented,
      reservedNames = [],
      reservedOpNames = [],
      stringTy = \_ _ -> False,
      stringVal = const Nothing,
      intTy = \_ _ -> True,
      intVal = const Nothing,
      floatTy = \_ _ -> False, -- Circuits does not support floats
      floatVal = const Nothing
    }

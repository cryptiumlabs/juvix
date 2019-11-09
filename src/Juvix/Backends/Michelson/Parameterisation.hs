module Juvix.Backends.Michelson.Parameterisation where

import qualified Juvix.Core.Erased.Types as C
import qualified Juvix.Core.Types as C
import Juvix.Library
import qualified Michelson.Untyped as M

type Term = C.Term PrimVal

type Type = C.Type PrimTy

data PrimTy
  = PrimTy M.Type

data PrimVal
  = PrimVal ()

michelson ∷ C.Parameterisation PrimTy PrimVal
michelson = undefined

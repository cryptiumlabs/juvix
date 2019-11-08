-- | Module for predefined constants
module Juvix.Backends.LLVM.Codegen.Constants where

import Juvix.Library hiding (Type)
import qualified LLVM.AST.Type as Type

portLength ∷ Num p ⇒ p
portLength = 32

-- | An abbreviation for 'IntegerType' 8
i4 ∷ Type.Type
i4 = Type.IntegerType 4

-- TODO ∷ change this based on the machine, C++ pargma?

-- | Use 64 bit ints
int ∷ Type.Type
int = Type.IntegerType 64

double ∷ Type.Type
double = Type.FloatingPointType Type.DoubleFP

-- Finite fields example
module Test.AnnTerm where

import Data.Curve.Weierstrass.BLS12381 (Fr)
import Juvix.Backends.Plonk (AnnTerm, FFType, PrimVal (..))
import qualified Juvix.Backends.Plonk as P
import Juvix.Core.ErasedAnn
import Juvix.Library hiding (Type, exp)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage

sig :: FFType Fr
sig =
  Pi (SNat 1) (PrimTy P.PField) $
    Pi (SNat 1) (PrimTy P.PField) $
      PrimTy P.PField

add, sub, mul, exp :: AnnTerm Fr
add = Ann Omega sig $ Prim PAdd
sub = Ann Omega sig $ Prim PSub
mul = Ann Omega sig $ Prim PMul
exp = Ann Omega sig $ Prim PExp

eq :: AnnTerm Fr
eq = Ann Omega sig $ Prim PAssertEq

val :: Fr -> AnnTerm Fr
val = Ann (SNat 1) (PrimTy P.PField) . Prim . PConst

var :: NameSymbol.T -> AnnTerm Fr
var = Ann (SNat 1) (PrimTy P.PField) . Var

app :: AnnTerm Fr -> [AnnTerm Fr] -> AnnTerm Fr
app f xs = Ann (SNat 2) (PrimTy P.PField) $ AppM f xs

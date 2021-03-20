-- Finite fields example
module Test.AnnTerm where

import Data.Curve.Weierstrass.BLS12381 (Fr)
import Juvix.Backends.Plonk (FF (..), FFAnnTerm, FFType, PrimVal (..))
import qualified Juvix.Backends.Plonk as P
import Juvix.Core.ErasedAnn
import Juvix.Library hiding (Type, exp)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage

-- TODO: Is this signature correct? Do we need it?
sig :: FFType
sig =
  Pi (SNat 1) (PrimTy FF)
    $ Pi (SNat 1) (PrimTy FF)
    $ PrimTy FF

add, sub, mul, exp :: FFAnnTerm Fr
add = Ann Omega sig $ Prim PAdd
sub = Ann Omega sig $ Prim PSub
mul = Ann Omega sig $ Prim PMul
exp = Ann Omega sig $ Prim PExp

eq = Ann Omega sig $ Prim PAssertEq

val :: Fr -> FFAnnTerm Fr
val = Ann (SNat 1) (PrimTy FF) . Prim . PConst

var :: NameSymbol.T -> FFAnnTerm Fr
var = Ann (SNat 1) (PrimTy FF) . Var

app :: FFAnnTerm Fr -> [FFAnnTerm Fr] -> FFAnnTerm Fr
app f xs = Ann (SNat 2) (PrimTy FF) $ AppM f xs

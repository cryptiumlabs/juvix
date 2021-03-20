module Main where

-- import qualified Juvix.Backends.Michelson as P

import Data.Curve.Weierstrass.BLS12381 (Fr)
-- import Juvix.Core.IR

import Juvix.Core.ErasedAnn
import Juvix.Library hiding (exp, minus)
import Juvix.Library.Usage as Usage hiding (minus)
import qualified Test.Compiler as Compiler
import qualified Test.Groth as Groth
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = T.defaultMain $ T.testGroup "Plonk tests" [Compiler.top, Groth.top]

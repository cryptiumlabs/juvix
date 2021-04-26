{-# LANGUAGE DeriveAnyClass #-}

module Test.Compiler where

import qualified Data.Aeson as A
import Data.Curve.Weierstrass.BLS12381 (Fr)
import Data.Field.Galois (GaloisField, PrimeField (..), toP)
import qualified Data.Map as Map
import Juvix.Backends.Plonk (FFAnnTerm, FFType, PrimVal (..))
import qualified Juvix.Backends.Plonk as P
import Juvix.Library (Natural, undefined, ($), (.))
import Juvix.Library hiding (Type, exp)
import qualified Test.Example.Polynomial as Example
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Data.Scientific as S

import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Core as Core

deriving instance Bits Fr
instance A.FromJSON Fr where
  parseJSON (A.Number n) = case S.floatingOrInteger n of
    Left floating -> panic $ "Can't parse floating :" <> show n
    Right f -> pure . toP $ toInteger f

instance A.ToJSON Fr where
  toJSON f = A.Number $ S.scientific (fromP f) 0


top :: T.TestTree
top =
  T.testGroup
    "Compiler tests"
    [polynomials]

polynomials :: T.TestTree
polynomials =
  T.testGroup
    "Polynomials"
    [ polynomial1 ]
    -- , testAnd
    -- , testOr
    -- , testXOr]

testOutput :: (GaloisField f, Bits f) => P.ArithCircuit f -> Map P.Wire f -> f -> T.Assertion
testOutput circuit inputs expectedOutput = expectedOutput T.@=? actualOutput
  where
    assignment = P.generateAssignment circuit inputs
    actualOutput = maybe (panic $ "Not output found: " <> show assignment) snd (head $ Map.toList (P.assignmentOutput assignment)) -- There should be only one output

mkInputs :: [f] -> Map P.Wire f
mkInputs inputs = Map.fromList $ zipWith (\k v -> (P.InputWire k, v)) [0 ..] inputs

polynomial1 :: T.TestTree
polynomial1 = T.testCase "\\x y -> x^3 - 2x^2 + 4 = y" (testOutput Example.circuitPolynomial1 inputs output)
  where
    inputs = mkInputs [1, 3]
    output = 1 -- true!

compile :: FilePath  -> Pipeline.Pipeline (FFAnnTerm Fr)
compile fin = do
  t <- liftIO $ readFile fin
  parsed <- Pipeline.parse (P.BPlonk :: P.BPlonk Fr) t
  s <- Pipeline.typecheck @(P.BPlonk Fr) parsed
  pure $ Core.toRaw s

orExample :: Pipeline.Pipeline (FFAnnTerm Fr)
orExample = compile "Example/Juvix/Or.ju"

andExample :: Pipeline.Pipeline (FFAnnTerm Fr)
andExample = compile "Example/Juvix/And.ju"

xorExample :: Pipeline.Pipeline (FFAnnTerm Fr)
xorExample = compile "Example/Juvix/XOr.ju"




module Main where

import Backends.ArithmeticCircuit
import Backends.LLVM
import Backends.Michelson
import Control.Exception
import CoreConv
import CoreParser
import CoreTypechecker
import EAC2
import Erasure
import qualified Frontend as Frontend
import qualified Juvix.Core.IR as IR
import Juvix.Core.Parameterisations.All as All
import Juvix.Core.Parameterisations.Naturals as Nat
import Juvix.Core.Parameterisations.Unit
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.Ingredients.Basic as T

coreTests ∷ T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ coreCheckerEval,
      coreConversions,
      coreParser
    ]

backendTests ∷ T.TestTree
backendTests =
  T.testGroup
    "Backend tests"
    [ backendCircuit,
      backendLLVM,
      backendMichelson
    ]

frontEndTests ∷ T.TestTree
frontEndTests =
  T.testGroup
    "frontend tests"
    [Frontend.allParserTests]

allCheckedTests ∷ T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ coreTests,
      backendTests,
      eac2Tests,
      erasureTests,
      frontEndTests
    ]

main ∷ IO ()
main = T.defaultMain allCheckedTests

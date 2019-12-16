module Main where

import Backends.ArithmeticCircuit
import Backends.LLVM
import Backends.Michelson
import Control.Exception
import CoreConv
import CoreParser
import CoreTypechecker
import EAC2
import qualified Juvix.Core.IR as IR
import Juvix.Core.Parameterisations.All as All
import Juvix.Core.Parameterisations.Naturals
import Juvix.Core.Parameterisations.Unit
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

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

allCheckedTests ∷ T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ coreTests,
      backendTests,
      eac2Tests
    ]

main ∷ IO ()
main =
  T.defaultMain allCheckedTests
    `Control.Exception.catch` ( \e → do
                                  if e == ExitSuccess
                                    then putByteString "All tests passed."
                                    else
                                      putStr $
                                        concatMap
                                          IR.msg
                                          --TODO add newline (intersperse
                                          --  "\n"
                                          ( IR.typecheckerLog $
                                              snd (IR.exec (IR.typeTerm All.all 0 [] depIdentity depIdentityCompTy))
                                          )
                                  Juvix.Library.throwIO e
                              )

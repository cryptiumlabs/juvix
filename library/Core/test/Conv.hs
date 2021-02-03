module Conv where

import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Translate as Trans
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldConvertHR :: T.TestName -> HR.Term () () -> IR.Term () () -> T.TestTree
shouldConvertHR msg hr ir =
  T.testCase msg (ir T.@=? Trans.hrToIR hr)

shouldConvertIR :: T.TestName -> IR.Term () () -> HR.Term () () -> T.TestTree
shouldConvertIR msg ir hr =
  T.testCase msg (hr T.@=? Trans.irToHR ir)

coreConversions :: T.TestTree
coreConversions =
  T.testGroup
    "Core Conversions"
    [ hrToIrConversion,
      irToHrConversion
    ]

hrToIrConversion :: T.TestTree
hrToIrConversion =
  T.testGroup
    "human readable to intermediate representation"
    [ shouldConvertHR
        "λx. x"
        (HR.Lam "x" (HR.Elim (HR.Var "x")))
        (IR.Lam (IR.Elim (IR.Bound 0))),
      shouldConvertHR
        "λx y. x"
        (HR.Lam "x" (HR.Lam "y" (HR.Elim (HR.Var "x"))))
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 1)))),
      shouldConvertHR
        "λx y. y"
        (HR.Lam "x" (HR.Lam "y" (HR.Elim (HR.Var "y"))))
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 0)))),
      shouldConvertHR
        "λf. f (λx. x) (λy. y)"
        ( HR.Lam "f"
            $ HR.Elim
            $ HR.Var "f"
              `HR.App` HR.Lam "x" (HR.Elim $ HR.Var "x")
              `HR.App` HR.Lam "y" (HR.Elim $ HR.Var "x")
        )
        ( IR.Lam
            $ IR.Elim
            $ IR.Bound 0
              `IR.App` IR.Lam (IR.Elim $ IR.Bound 0)
              `IR.App` IR.Lam (IR.Elim $ IR.Free (IR.Global "x"))
        )
    ]

irToHrConversion :: T.TestTree
irToHrConversion =
  T.testGroup
    "intermediate representation to human readable"
    [ shouldConvertIR
        "λ. 0"
        (IR.Lam (IR.Elim (IR.Bound 0)))
        (HR.Lam "0" (HR.Elim (HR.Var "0"))),
      shouldConvertIR
        "λ. λ. 1"
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 1))))
        (HR.Lam "0" (HR.Lam "1" (HR.Elim (HR.Var "0")))),
      shouldConvertIR
        "λ. λ. 0"
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 0))))
        (HR.Lam "0" (HR.Lam "1" (HR.Elim (HR.Var "1"))))
    ]

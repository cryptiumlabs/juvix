module Conv where

import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Translate as Trans
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldConvertHR :: HR.Term () () -> IR.Term () () -> T.TestTree
shouldConvertHR hr ir =
  T.testCase (show hr <> " should convert to " <> show ir) (ir T.@=? Trans.hrToIR hr)

shouldConvertHRWith ::
  Traversable t => t (HR.Pattern () ()) -> HR.Term () () -> IR.Term () () -> T.TestTree
shouldConvertHRWith pats hr ir =
  T.testCase (show hr <> " should convert to " <> show ir) (ir T.@=? convertWith pats hr)

convertWith :: Traversable t => t (HR.Pattern () ()) -> HR.Term () () -> IR.Term () ()
convertWith pats = Trans.hrToIRWith (snd (Trans.hrPatternsToIR pats))

shouldConvertIR :: IR.Term () () -> HR.Term () () -> T.TestTree
shouldConvertIR ir hr =
  T.testCase (show ir <> " should convert to " <> show hr) (hr T.@=? Trans.irToHR ir)

coreConversions :: T.TestTree
coreConversions =
  T.testGroup
    "Core Conversions"
    [ hrToirConversion,
      irTohrConversion
    ]

hrToirConversion :: T.TestTree
hrToirConversion =
  T.testGroup
    "Converting Human Readable form to Intermediate Readable form"
    [ shouldConvertHR
        (HR.Lam "x" (HR.Elim (HR.Var "x")))
        (IR.Lam (IR.Elim (IR.Bound 0))),
      shouldConvertHR
        (HR.Lam "x" (HR.Lam "y" (HR.Elim (HR.Var "x"))))
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 1)))),
      shouldConvertHR
        (HR.Lam "x" (HR.Lam "y" (HR.Elim (HR.Var "y"))))
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 0)))),
      shouldConvertHR
        ( HR.Lam "f" $
            HR.Elim $
              HR.Var "f"
                `HR.App` HR.Lam "x" (HR.Elim $ HR.Var "x")
                `HR.App` HR.Lam "y" (HR.Elim $ HR.Var "x")
        )
        ( IR.Lam $
            IR.Elim $
              IR.Bound 0
                `IR.App` IR.Lam (IR.Elim $ IR.Bound 0)
                `IR.App` IR.Lam (IR.Elim $ IR.Free (Core.Global "x"))
        ),
      shouldConvertHRWith
        [HR.PVar "a", HR.PVar "hi"]
        (HR.Elim (HR.Var "a"))
        (IR.Elim (IR.Free (Core.Pattern 0)))
    ]

irTohrConversion :: T.TestTree
irTohrConversion =
  T.testGroup
    "Converting Intermediate Readable form to Human Readable form"
    [ shouldConvertIR
        (IR.Lam (IR.Elim (IR.Bound 0)))
        (HR.Lam "a" (HR.Elim (HR.Var "a"))),
      shouldConvertIR
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 1))))
        (HR.Lam "a" (HR.Lam "b" (HR.Elim (HR.Var "a")))),
      shouldConvertIR
        (IR.Lam (IR.Lam (IR.Elim (IR.Bound 0))))
        (HR.Lam "a" (HR.Lam "b" (HR.Elim (HR.Var "b"))))
    ]

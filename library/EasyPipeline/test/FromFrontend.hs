module FromFrontend where

import qualified Easy
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import qualified Juvix.ToCore.Types as Types
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "From Frontend Tests:"
    [patternVarTests]

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------

patternVarTests =
  T.testGroup
    "pattern Var Tests"
    [ T.testCase
        "pattern vars should be tracked"
        ( do
            pattern' <- rawPattern
            pattern' T.@=? rawPatternShouldBe
        )
    ]
  where
    rawPattern = do
      Right x <- Easy.coreify "sig foo : int let foo x = x" Easy.defMichelson
      let Just
            ( Types.CoreDef
                ( IR.RawGFunction
                    IR.RawFunction
                      { IR.rawFunClauses = IR.RawFunClause {IR.rawClauseBody = body} :| []
                      }
                  )
              ) =
              Easy.lookupCoreFunction x Easy.defMichelson "foo"
      pure body
    rawPatternShouldBe =
      IR.Elim (IR.Free (IR.Pattern 0))

-- |
-- Temporary conversion from the Sexpression syntax to the ML syntax
module Conversion.ML where

-- for local testing as development only

import Juvix.Conversion.ML as ML
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types.Base as Frontend
import qualified Juvix.FrontendDesugar as MLPasses
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Target
import Juvix.Library hiding (product, sum)
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error)

top :: T.TestTree
top =
  T.testGroup
    "passes agree upon results:"
    []

shouldBeTheSame :: T.TestName -> ByteString -> T.TestTree
shouldBeTheSame name str =
  T.testCase
    ("passes agree upon:" <> name)
    (parseDesugarML str T.@=? fmap ML.op (parseDesugarSexp str))

------------------------------------------------------------
-- Tests
------------------------------------------------------------

functionTests :: T.TestTree
functionTests =
  T.testGroup
    "function tests"
    [ shouldBeTheSame "basic" "let foo = 3",
      shouldBeTheSame "guards" "let foo x | x == 3 = 5"
    ]

------------------------------------------------------------
-- Test Helper functions
------------------------------------------------------------

parseDesugarML :: ByteString -> [Target.TopLevel]
parseDesugarML =
  MLPasses.op . ignoreHeader . Parser.parse

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"

ignoreRight :: Either a p -> p
ignoreRight (Right x) = x
ignoreRight (Left _) = error "not right"

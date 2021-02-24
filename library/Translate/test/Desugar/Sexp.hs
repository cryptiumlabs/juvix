module Desugar.Sexp where

import qualified Juvix.Desugar.Passes as Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as Trans
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (head)
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error, head)

condWorksAsExpected :: T.TestTree
condWorksAsExpected =
  T.testGroup
    "cond desugar tests"
    [ T.testCase
        "recursive conds work"
        (expected T.@=? fmap Desugar.condTransform form)
    ]
  where
    form =
      Sexp.parse
        "(:defun foo (x)\
        \   (:cond ((:infix + x 3) 2) \
        \          ((bar true)     (:paren (:cond ((bar false) 3) (else 5)))) \
        \          (else           3)))"
    expected =
      Sexp.parse
        "(:defun foo (x) \
        \   (if (:infix + x 3)\
        \   2\
        \   (if (bar true)\
        \       (:paren (if (bar false) 3 (if else 5))) \
        \       (if else 3))))"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

singleEleErr :: Functor f => f (Types.Header Types.TopLevel) -> f Sexp.T
singleEleErr = fmap (Trans.transTopLevel . head . noHeaderErr)

noHeaderErr :: Types.Header topLevel -> [topLevel]
noHeaderErr (Types.NoHeader xs) = xs
noHeaderErr _ = error "imporper form"

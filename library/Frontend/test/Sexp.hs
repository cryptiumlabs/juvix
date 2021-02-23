module Sexp (top) where

import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as Trans
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (head)
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error, head)

--------------------------------------------------------------------------------
-- Exported top level test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "top level tests"
    [ topLevelLet,
      topLevelSig,
      topLevelType
    ]

--------------------------------------------------------------------------------
-- Top Level Tests
--------------------------------------------------------------------------------

topLevelLet :: T.TestTree
topLevelLet =
  T.testGroup
    "topLevel Let Tests"
    [T.testCase "guard test" (guard T.@=? guardExpected)]
  where
    guard =
      Parser.parseOnly
        "let foo x \
        \  | x == 2 = 3 + 4 + x \
        \  | else   = x + 2"
        |> singleEleErr
    guardExpected =
      Sexp.parse
        "(:defun foo (x) \
        \   (:cond \
        \    ((:infix == x 2) (:infix + 3 (:infix + 4 x))) \
        \    (else            (:infix + x 2))))"

topLevelSig :: T.TestTree
topLevelSig =
  T.testGroup
    "topLevel Sig tests"
    [ T.testCase "basic" (basic T.@=? basicExpected),
      T.testCase "usage" (usage T.@=? usageExpected)
    ]
  where
    basic =
      Parser.parseOnly "sig foo : int -> int -> int" |> singleEleErr
    basicExpected =
      Sexp.parse "(:defsig foo (:infix -> int (:infix -> int int)))"
    usage =
      Parser.parseOnly "sig foo 0 : int -> int -> int" |> singleEleErr
    -- may change in the future
    usageExpected =
      Sexp.parse "(:defsig foo (:usage 0 (:infix -> int (:infix -> int int))))"

topLevelType :: T.TestTree
topLevelType =
  T.testGroup
    "Types parser"
    [ T.testCase "record" (record T.@=? recordExpected),
      T.testCase "∑ Complex" (sumC T.@=? sumCExpected),
      T.testCase "signature" (sig T.@=? sigExpected)
    ]
  where
    record =
      Parser.parseOnly
        "type foo x y z = \
        \ { y-axis : y \
        \ , x-axis : x \
        \ , z-axis : z }"
        |> singleEleErr
    recordExpected =
      Sexp.parse
        "(type foo (x y z) \
        \ (:record \
        \  y-axis y \
        \  x-axis x \
        \  z-axis z))"
    sumC =
      Parser.parseOnly
        "type foo x y z = \
        \  | Foo {y-axis : y, x-axis : x, z-axis : z} \
        \  | Bar (var1 : x) (var2 : y) \
        \  | Car : hd : x -> cdr : foo y -> foo x y z"
        |> singleEleErr
    sumCExpected =
      Sexp.parse
        "(type foo (x y z) \
        \  (Foo (:record y-axis y x-axis x z-axis z)) \
        \  (Bar (:paren (:infix : var1 x)) \
        \       (:paren (:infix : var2 y))) \
        \  (Car (:arrow \
        \        (:infix : hd \
        \                (:infix -> x \
        \                       (:infix : cdr \
        \                       (:infix -> (foo y) (foo x y z))))))))"
    sig =
      Parser.parseOnly "type foo x y z : typ -> typ -> typ = Foo a b c"
        |> singleEleErr
    sigExpected =
      Sexp.parse
        "(type (foo :type (:infix -> typ (:infix -> typ typ))) (x y z) \
        \   (Foo a b c))"

--------------------------------------------------------------------------------
-- Expression Tests
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

singleEleErr :: Functor f => f (Types.Header Types.TopLevel) -> f Sexp.T
singleEleErr = fmap (Trans.transTopLevel . head . noHeaderErr)

noHeaderErr :: Types.Header topLevel -> [topLevel]
noHeaderErr (Types.NoHeader xs) = xs
noHeaderErr _ = error "imporper form"

module Test.Contextify (top) where

import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Environment as Env
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as Contextify
import qualified Juvix.Core.Common.Context as Context
import Juvix.Library (Either (Left, Right), Maybe (Just), ($))
import qualified Juvix.Library.Sexp as Sexp
import Test.Sexp.Helpers
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "testing desugaring passes functions"
    [moduleResolution, infixResolution]

moduleResolution :: T.TestTree
moduleResolution =
  T.testGroup
    "Testing module resolution and binders"
    [ T.testCase "ignore binder on function arguments" $ do
        Right t <- contextualizeFoo "open A let fi a = a"
        let Right expected = Sexp.parse "(:lambda-case ((a) a))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "opening actually resolves names properly" $ do
        Right t <- contextualizeFoo "open A let fi x = a"
        let Right expected = Sexp.parse "(:lambda-case ((x) TopLevel.A.a))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "λ properly updates closure" $ do
        Right t <- contextualizeFoo "open A let fi = \\a -> a"
        let Right expected = Sexp.parse "(:lambda-case (() (:lambda (a) a)))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "match properly udpates closure" $ do
        Right t <-
          contextualizeFoo
            "open A let fi = case a of | Cons a (Cons x y) -> add a x y"
        let Right expected =
              Sexp.parse
                "(:lambda-case \
                \  (() (case TopLevel.A.a ((Cons a (Cons x y)) (add a x y)))))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "defining an imported function just shadows" $ do
        Right t <- contextualizeFoo "open A let fi = x let x = 2"
        let Right expected = Sexp.parse "(:lambda-case (() x))"
        unwrapLookup "fi" t T.@=? Just expected,
      T.testCase "ambiguous imports error" $ do
        t <- contextualizeFooAmbi "open A open B let fi = 2"
        Left (Contextify.Resolve (Contextify.AmbiguousSymbol "+")) T.@=? t
    ]

infixResolution :: T.TestTree
infixResolution =
  T.testGroup
    "Testing infix resolution and clashses"
    [ T.testCase "two forms with infix will error" $ do
        t <- contextualizeFoo "open A let fi a = a ** a *** a"
        empt <- Context.empty "foo"
        case t of
          Left (Contextify.PassErr (Env.Clash p1 p2)) ->
            Left (Contextify.PassErr (Env.Clash p1 p2)) T.@=? t
          _ ->
            t T.@=? Right empt,
      T.testCase "two forms with infix will error" $ do
        Right t <- contextualizeFoo "open A let fi a = 1 * 2 + 3"
        let Right expected = Sexp.parse "(:lambda-case ((a) (TopLevel.A.+ (TopLevel.A.* 1 2) 3)))"
        Just expected T.@=? unwrapLookup "fi" t
    ]

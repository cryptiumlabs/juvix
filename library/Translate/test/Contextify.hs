module Contextify (top) where

import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Environment as Env
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as Contextify
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types.Base as Frontend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
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

----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

unwrapLookup :: NameSymbol.T -> Context.T a ty sumRep -> Maybe a
unwrapLookup symbol ctx =
  case Context.lookup symbol ctx >>| Context.extractValue of
    Just ((Context.Def Context.D {defTerm})) ->
      Just defTerm
    _ -> Nothing

contextualizeFoo ::
  ByteString -> IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextualizeFoo byte =
  Contextify.op
    ( ( "A",
        parseDesugarSexp
          "declare infixl (+) 8 \
          \ let (+) = 3 \
          \ declare infixl (*) 9 \
          \ let (*) = 3 \
          \ declare infix (**) 7 \
          \ let (**) = 3 \
          \ declare infix (***) 7 \
          \ let (**) = 3 \
          \ let a = 3 \
          \ let x = 2 "
      )
        :| [("Foo", parseDesugarSexp byte)]
    )

contextualizeFooAmbi ::
  ByteString -> IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextualizeFooAmbi byte =
  Contextify.op
    ( ( "A",
        parseDesugarSexp "declare infixl (+) 9 let (+) = 3 "
      )
        :| [ ("B", parseDesugarSexp "declare infixl (+) 9 let (+) = 3"),
             ("Foo", parseDesugarSexp byte)
           ]
    )

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"

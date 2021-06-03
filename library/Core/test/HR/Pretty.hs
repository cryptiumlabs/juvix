{-# OPTIONS_GHC -Wmissing-exported-signatures #-}

module HR.Pretty
  ( top,
  )
where

import Data.String (IsString (..), String)
import Juvix.Core.HR as HR
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import Juvix.Library.Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Text.Show

top :: T.TestTree
top =
  -- TODO test syntax highlighting stuff
  T.testGroup
    "HR pretty printing"
    [ atomTests,
      -- TODO pp for michelson primitives
      bindTests,
      lamTests,
      pairTests,
      letTests,
      appTests,
      annTests
    ]

atomTests :: T.TestTree
atomTests =
  T.testGroup
    "Atomic terms"
    [ T.testCase "*" $
        prettyAt 1000 (Star 0) @?= "* 0",
      T.testCase "UnitTy" $
        prettyAt 10 UnitTy @?= "Unit",
      T.testCase "Unit" $
        prettyAt 10 Unit @?= "⌷",
      T.testCase "foo" $
        "foo" :| []
          @@?= "foo",
      T.testCase "foo.bar.baz.quox" $
        "foo" :| ["bar", "baz", "quox"]
          @@?= "foo.bar.baz.quox"
    ]
  where
    infix 1 @@?=
    ns @@?= str = PP.render (PP.pretty0 (ns :: NameSymbol.T)) @?= str

bindTests :: T.TestTree
bindTests =
  T.testGroup
    "Binding types"
    [ T.testCase "xAB wide" $
        prettyAt 1000 xAB
          @?= "Π (ω | x : A) → B x",
      T.testCase "xAB narrow" $
        prettyAt 15 xAB
          @?= "Π (ω | x : A) →\n\
              \  B x",
      T.testCase "xAyBC wide" $
        prettyAt 1000 xAyBC
          @?= "Π (ω | x : A) → Π (ω | y : B) → C x y",
      T.testCase "xAyBC narrow" $
        prettyAt 20 xAyBC
          @?= "Π (ω | x : A) →\n\
              \Π (ω | y : B) →\n\
              \  C x y",
      T.testCase "xAyBC' wide" $
        prettyAt 1000 xAyBC'
          @?= "Π (ω | x : A) → Σ (ω | y : B) → C x y",
      T.testCase "xAyBC' narrow" $
        prettyAt 20 xAyBC'
          @?= "Π (ω | x : A) →\n\
              \Σ (ω | y : B) →\n\
              \  C x y"
    ]

xAB =
  Pi Omega "x" "A" $
    Elim $
      "B" `App` "x"

xAyBC =
  Pi Omega "x" "A" $
    Pi Omega "y" "B" $
      Elim $
        "C" `App` "x" `App` "y"

xAyBC' =
  Pi Omega "x" "A" $
    Sig Omega "y" "B" $
      cxy

cxy = Elim cxy'

cxy' = "C" `AppE` "x" `AppE` "y"

lamTests :: T.TestTree
lamTests =
  T.testGroup
    "Lambda"
    [ T.testCase "lam1 wide" $
        prettyAt 1000 lam1
          @?= "λ z → z",
      T.testCase "lam1 narrow" $
        prettyAt 5 lam1
          @?= "λ z →\n\
              \    z",
      T.testCase "lam2 wide" $
        prettyAt 1000 lam2
          @?= "λ y z → ‹y, z›",
      T.testCase "lam2 narrow" $
        prettyAt 10 lam2
          @?= "λ y z →\n\
              \    ‹y, z›",
      T.testCase "lam3" $
        prettyAt 18 lam3
          @?= "λ verylongname1\n\
              \  verylongname2\n\
              \  verylongname3 →\n\
              \    ‹y, z›"
    ]

lam1 = Lam "z" $ Elim "z"

lam2 = Lam "y" $ Lam "z" $ "y" `Pair` "z"

lam3 =
  Lam "verylongname1" $
    Lam "verylongname2" $
      Lam "verylongname3" $
        "y" `Pair` "z"

pairTests :: T.TestTree
pairTests =
  T.testGroup
    "Pairs"
    [ T.testCase "two wide" $
        prettyAt 1000 two
          @?= "‹f x y, g x y›",
      T.testCase "two narrow" $
        prettyAt 10 two
          @?= "‹f x y,\n\
              \ g x y›",
      T.testCase "three wide" $
        prettyAt 1000 three
          @?= "‹f x y, g x y, f x y›",
      T.testCase "three narrow" $
        prettyAt 10 three
          @?= "‹f x y,\n\
              \ g x y,\n\
              \ f x y›",
      T.testCase "nested fun types" $
        prettyAt 20 two'
          @?= "‹Π (ω | x : A) →\n\
              \ Π (ω | y : B) →\n\
              \   C x y,\n\
              \ Π (ω | x : A) →\n\
              \ Π (ω | y : B) →\n\
              \   C x y›"
    ]

fxy' = "f" `AppE` "x" `AppE` "y"

gxy' = "g" `AppE` "x" `AppE` "y"

fxy = Elim fxy'

gxy = Elim gxy'

two = fxy `Pair` gxy

two' = xAyBC `Pair` xAyBC

three = fxy `Pair` (gxy `Pair` fxy)

letTests :: T.TestTree
letTests =
  T.testGroup
    "Let"
    [ T.testCase "let1 wide" $
        prettyAt 1000 let1
          @?= "let 2 | a = f x y in ‹a, a›",
      T.testCase "let1 med" $
        prettyAt 25 let1
          @?= "let 2 | a = f x y in\n\
              \‹a, a›",
      T.testCase "let1 narrow" $
        prettyAt 10 let1
          @?= "let 2 | a =\n\
              \  f x y in\n\
              \‹a, a›",
      T.testCase "let2 wide" $
        prettyAt 1000 let2
          @?= "let 2 | a = f x y in let 1 | b = g x y in ‹a, a, b›",
      T.testCase "let2 med" $
        prettyAt 25 let2
          @?= "let 2 | a = f x y in\n\
              \let 1 | b = g x y in\n\
              \‹a, a, b›",
      T.testCase "let2 narrow" $
        prettyAt 10 let2
          @?= "let 2 | a =\n\
              \  f x y in\n\
              \let 1 | b =\n\
              \  g x y in\n\
              \‹a, a, b›"
    ]

let1 = Let (SNat 2) "a" fxy' $ "a" `Pair` "a"

let2 =
  Let (SNat 2) "a" fxy' $
    Let (SNat 1) "b" gxy' $
      "a" `Pair` ("a" `Pair` "b")

appTests :: T.TestTree
appTests =
  T.testGroup
    "Applications"
    [ T.testCase "app1 wide" $
        prettyAt 1000 app1
          @?= "zipApply3 ‹f, g, h› ‹x, y, z›",
      T.testCase "app1 narrow" $
        prettyAt 15 app1
          @?= "zipApply3\n\
              \  ‹f, g, h›\n\
              \  ‹x, y, z›",
      T.testCase "app2 wide" $
        prettyAt 1000 app2
          @?= "merge (zipApply3 ‹f, g, h› ‹x, y, z›) (zipApply3 ‹f, g, h› ‹x, y, z›)",
      T.testCase "app2 med" $
        prettyAt 40 app2
          @?= "merge\n\
              \  (zipApply3 ‹f, g, h› ‹x, y, z›)\n\
              \  (zipApply3 ‹f, g, h› ‹x, y, z›)",
      T.testCase "app2 narrow" $
        prettyAt 20 app2
          @?= "merge\n\
              \  (zipApply3\n\
              \     ‹f, g, h›\n\
              \     ‹x, y, z›)\n\
              \  (zipApply3\n\
              \     ‹f, g, h›\n\
              \     ‹x, y, z›)"
    ]

app1 =
  "zipApply3"
    `App` ("f" `Pair` ("g" `Pair` "h"))
    `App` ("x" `Pair` ("y" `Pair` "z"))

app2 = "merge" `AppE` app1 `AppE` app1

annTests :: T.TestTree
annTests =
  T.testGroup
    "Annotations"
    [ T.testCase "ann wide" $
        prettyAt 1000 ann
          @?= "(ω | f x y : C x y : * 0)",
      T.testCase "ann narrow" $
        prettyAt 10 ann
          @?= "(ω | f x y\n\
              \   : C x y\n\
              \   : * 0)"
    ]

ann = Ann Omega fxy cxy 0

pattern AppE f e = App f (Elim e)

instance IsString (Elim primTy primVal) where fromString x = Var $ fromString x

instance IsString (Term primTy primVal) where fromString x = Elim $ fromString x

-- present errors more legibly: don't escape characters and instead put the
-- string on its own line(s), indented
newtype S = S String deriving (Eq)

instance Show S where
  show (S s) = "「\n  " ++ indent s ++ "\n」"
    where
      indent = concatMap \case '\n' -> "\n  "; c -> [c]

infix 1 @?=

(@?=) :: String -> String -> T.Assertion
a @?= b = S a T.@?= S b

prettyAt :: (a ~ tm HR.T Nat.Ty Nat.Val, PP.PrettySyntax a) => Int -> a -> String
prettyAt n x =
  PP.renderWith (PP.defaultOptions {PP.optsPageWidth = n}) $
    PP.pretty0 x

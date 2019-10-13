--Tests for the type checker and evaluator in Core/MainLang.hs.
module MainLang where

import Juvix.Core.MainLang
import Juvix.Core.Parser
import Juvix.Core.SemiRing
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude

identity ∷ CTerm
identity = Lam (Conv (Bound 0))

identityCompTy ∷ Annotation
identityCompTy =
  ( SNat 1,
    VPi (SNat 1) VNats (const (VNats))
  )

identityContTy ∷ Annotation
identityContTy =
  ( SNat 0,
    VPi (SNat 0) VNats (const (VNats))
  )

test_identity_computational ∷ T.TestTree
test_identity_computational = shouldCheck identity identityCompTy

test_identity_contemplation ∷ T.TestTree
test_identity_contemplation = shouldCheck identity identityContTy

test_nats_type_star0 ∷ T.TestTree
test_nats_type_star0 = shouldCheck Nats (SNat 0, VStar 0)

test_nat1 ∷ T.TestTree
test_nat1 = shouldInfer (Nat 1) (Omega, VNats)

--unit tests for cType
shouldCheck ∷ CTerm → Annotation → T.TestTree
shouldCheck term ann =
  T.testCase (show term <> " should check as type " <> show ann) $
    cType 0 [] term ann T.@=? Right ()

--unit tests for iType
shouldInfer ∷ ITerm → Annotation → T.TestTree
shouldInfer term ann =
  T.testCase (show term <> " should infer to type " <> show ann) $
    iType0 [] term T.@=? Right ann

one ∷ CTerm
one =
  Lam
    $ Lam
    $ Conv
    $ App
      (Bound 1)
      (Conv (Bound 0))

oneCompTy ∷ Annotation
oneCompTy =
  ( SNat 1,
    VPi
      (SNat 1)
      ( VPi
          (SNat 1)
          VNats
          (const VNats)
      )
      ( const
          ( VPi
              (SNat 1)
              VNats
              (const VNats)
          )
      )
  )

two ∷ CTerm
two =
  Lam
    $ Lam
    $ Conv
    $ App
      (Bound 1)
      ( Conv
          ( App
              (Bound 1)
              (Conv (Bound 0))
          )
      )

twoCompTy ∷ Annotation
twoCompTy =
  ( SNat 1,
    VPi
      (SNat 2)
      ( VPi
          (SNat 1)
          VNats
          (const VNats)
      )
      ( const
          ( VPi
              (SNat 1)
              VNats
              (const VNats)
          )
      )
  )

-- property tests of type checker:
-- the term's inferred type equals to the input type
-- property test of evaluator:
-- \x.x (any term) evaluates to (any term evaluated)
{- lamProp ∷ CTerm → Env → Property
lamProp cterm env = App (cEval (Lam Bound 0) env) cterm == cterm-}
-- any constant term evaluates to itself
-- constProp ∷ CTerm → Env → Bool
-- constProp (Star i) env = cEval (Star i) env == VStar i
-- constProp Nats env     = cEval Nats env == VNats
-- constProp _ _          = True --Not testing non-const terms

{- TODO need to combine generators to generate
   CTerms http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html
instance Arbitrary CTerm where
  arbitrary = CTerm -}

-- unit tests for cterm parser.
shouldParse ∷ String → CTerm → T.TestTree
shouldParse term parsed =
  T.testCase (show term <> " should parse as " <> show parsed) $
    parseString (parseWhole cterm) term T.@=? Just parsed

test_star_n ∷ T.TestTree
test_star_n = shouldParse "* 0" (Star 0)

test_primitive_type_Nats ∷ T.TestTree
test_primitive_type_Nats = shouldParse "Nat" Nats

test_dependent_fun ∷ T.TestTree
test_dependent_fun =
  shouldParse
    "[Π] 1 * 0 * 0"
    (Pi (SNat 1) (Star 0) (Star 0))

test_lam_identity ∷ T.TestTree
test_lam_identity = shouldParse "\\x. Bound 0" (Lam (Conv (Bound 0)))

test_conversion ∷ T.TestTree
test_conversion = shouldParse "Conv 0" (Conv (Nat 0))

test_silent_convert_nat ∷ T.TestTree
test_silent_convert_nat = shouldParse "0" (Conv (Nat 0))

test_silent_convert_Bound ∷ T.TestTree
test_silent_convert_Bound = shouldParse "Bound 0" (Conv (Bound 0))

test_silent_convert_Free ∷ T.TestTree
test_silent_convert_Free = shouldParse "Free (Global aStringName)" (Conv (Free (Global "aStringName")))

test_silent_convert_App ∷ T.TestTree
test_silent_convert_App =
  -- doesn't make sense now because there is no ITerm that is a function atm.
  shouldParse "App Bound 0 Nat" (Conv (App (Bound 0) Nats))

test_silent_convert_Ann ∷ T.TestTree
test_silent_convert_Ann = shouldParse "w Nat : * 0" (Conv (Ann Omega Nats (Star 0)))

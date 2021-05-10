{-# LANGUAGE OverloadedLists #-}

module Unify (coreUnifier) where

import Juvix.Core.IR.Types (Globals, pattern VFree)
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import Juvix.Core.Unify
import qualified Juvix.Core.Unify.MetaVar as Meta
import Juvix.Library
import qualified Test.Tasty as T
import Test.Tasty.HUnit (assertFailure, (@?=))
import qualified Test.Tasty.HUnit as T

type NGlobals = Globals Nat.Ty Nat.Val

type NValue = Value Nat.Ty Nat.Val

type NError = Error Nat.Ty Nat.Val

shouldHaveUnsolved ::
  T.TestName ->
  NGlobals ->
  -- | lhs of equation
  NValue ->
  -- | rhs of equation
  NValue ->
  -- | unsolved metas
  (Meta.MetaSet -> Bool) ->
  T.TestTree
shouldHaveUnsolved name g s t p =
  T.testCase ("⋯ " <> name) $
    case unifyV g s t of
      Left err -> assertFailure $ show err
      Right (Success σ u) ->
        unless (p u) $
          assertFailure $
            "unsolved: " <> show u <> "\n"
              <> "     lhs: "
              <> show (appSubstV σ s)
              <> "\n"
              <> "     lhs: "
              <> show (appSubstV σ t)
              <> "\n"
              <> "       σ: "
              <> show σ

shouldUnifyTo ::
  T.TestName ->
  NGlobals ->
  -- | lhs of equation
  NValue ->
  -- | rhs of equation
  NValue ->
  -- | desired result
  NValue ->
  T.TestTree
shouldUnifyTo name g s t r =
  T.testCase ("✓ " <> name) $
    case unifyV g s t of
      Left err -> assertFailure $ show err
      Right (Success σ u) -> do
        u @?= []
        appSubstV σ s @?= r
        appSubstV σ t @?= r

shouldError ::
  T.TestName ->
  NGlobals ->
  -- | lhs of equation
  NValue ->
  -- | rhs of equation
  NValue ->
  -- | error predicate
  (NError -> Bool) ->
  T.TestTree
shouldError name g s t p =
  T.testCase ("✗ " <> name) $
    case unifyV g s t of
      Left err -> unless (p err) $ assertFailure $ show err
      Right (Success σ u) ->
        assertFailure $
          "unexpected success\n"
            <> "unsolved: "
            <> show u
            <> "\n"
            <> "     lhs: "
            <> show (appSubstV σ s)
            <> "\n"
            <> "     lhs: "
            <> show (appSubstV σ t)
            <> "\n"
            <> "       σ: "
            <> show σ

isClash :: NError -> Bool
isClash = \case Clash _ _ -> True; _ -> False

isClashU :: NError -> Bool
isClashU = \case ClashU _ _ -> True; _ -> False

isOccurs :: NError -> Bool
isOccurs = \case Occurs _ _ -> True; _ -> False

-- TODO tests with dB vars and binders
coreUnifier :: T.TestTree
coreUnifier =
  T.testGroup
    "unifier"
    [ shouldUnifyTo
        "A ≟ A"
        globals
        (free "A")
        (free "A")
        (free "A"),
      shouldError
        "A ≟ B"
        globals
        (free "A")
        (free "B")
        isClash,
      shouldUnifyTo
        "α ≟ A"
        globals
        (VMeta α)
        (free "A")
        (free "A"),
      shouldHaveUnsolved
        "α ≟ α"
        globals
        (VMeta α)
        (VMeta α)
        (== [α]),
      shouldHaveUnsolved
        "α ≟ β"
        globals
        (VMeta α)
        (VMeta β)
        (not . Meta.nullS),
      shouldError
        "α ≟ F α"
        globals
        (VMeta α)
        (appF (VMeta α))
        isOccurs,
      shouldUnifyTo
        "⋆₀ ≟ ⋆₀"
        globals
        (VStar 0)
        (VStar 0)
        (VStar 0),
      shouldError
        "⋆₂ ≟ ⋆₁"
        globals
        (VStar 2)
        (VStar 1)
        isClash,
      shouldUnifyTo
        "ℕ ≟ ℕ"
        globals
        (VPrimTy Nat.Ty)
        (VPrimTy Nat.Ty)
        (VPrimTy Nat.Ty),
      shouldUnifyTo
        "1 ≟ 1"
        globals
        nat1
        nat1
        nat1,
      shouldError
        "1 ≟ 2"
        globals
        nat1
        nat2
        isClash,
      shouldUnifyTo
        "Π(0 x: A). B ≟ Π(0 x: A). B"
        globals
        π₀AB
        π₀AB
        π₀AB,
      shouldError
        "Π(1 x: A). B ≟ Π(0 x: A). B"
        globals
        π₁AB
        π₀AB
        isClashU,
      shouldUnifyTo
        "Π(1 x: α). B ≟ Π(1 x: A). β"
        globals
        π₁αB
        π₁Aβ
        π₁AB,
      shouldError
        "Π(1 x: α). B ≟ α"
        globals
        π₁αB
        (VMeta α)
        isOccurs,
      shouldUnifyTo
        "K α ≟ K 1  -- inj"
        globals
        (appK (VMeta α))
        (appK nat1)
        (appK nat1),
      shouldError
        "F α ≟ F 1  -- not inj"
        globals
        (appF (VMeta α))
        (appF nat1)
        isClash
    ]
  where
    globals =
      -- K injective, F not
      -- the fact they're ill typed doesn't matter for the tests
      [ ("F", IR.GAbstract $ IR.Abstract "F" IR.GOmega (VStar 0)),
        ("K", IR.GDataCon $ IR.DataCon "K" (VStar 0) Nothing)
      ]
    α : β : _ = Meta.metaVars
    nfree = NFree . Global
    free = VFree . Global
    nat1 = VPrim $ Nat.Val 1
    nat2 = VPrim $ Nat.Val 2
    π₀AB = VPi mempty (free "A") (free "B")
    π₁AB = VPi one (free "A") (free "B")
    π₁αB = VPi one (VMeta α) (free "B")
    π₁Aβ = VPi one (free "A") (VMeta β)
    vapp f s = VNeutral $ NApp f s
    appK = vapp (nfree "K")
    appF = vapp (nfree "F")

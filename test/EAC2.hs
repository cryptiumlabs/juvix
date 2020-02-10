module EAC2 where

import Juvix.Core.EAC.Check
import Juvix.Core.Erased.Types hiding (Term, Type, TypeAssignment)
import qualified Juvix.Core.Erased.Types as ET
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (Type, exp, link, reduce)
import qualified Juvix.Library.HashMap as Map
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

type Term = ET.Term ()

type Type = ET.Type ()

type TypeAssignment = ET.TypeAssignment ()

unitParam ∷ Parameterisation () ()
unitParam = Parameterisation (const (() :| [])) (\_ _ → Nothing) undefined undefined [] []

shouldBeTypeable ∷ Term → TypeAssignment → T.TestTree
shouldBeTypeable term assignment =
  T.testCase (show term <> " should be typeable in EAC") $ do
    valid ← validEal unitParam term assignment
    case valid of
      Right _ → return ()
      Left er → T.assertFailure (show er)

shouldNotBeTypeable ∷ Term → TypeAssignment → T.TestTree
shouldNotBeTypeable term assignment =
  T.testCase (show term <> " should not be typeable in EAC") $ do
    valid ← validEal unitParam term assignment
    case valid of
      Right _ → T.assertFailure "a satisfying assignment was found"
      Left _ → pure ()

eac2Tests ∷ T.TestTree
eac2Tests =
  T.testGroup
    "EAC2"
    []

--shouldBeTypeable idTerm idAssignment,
--shouldBeTypeable churchTwo churchAssignment,
--shouldBeTypeable churchThree churchAssignment,
--shouldNotBeTypeable counterexample counterexampleAssignment,
--shouldBeTypeable churchExp churchExpAssignment

idTerm ∷ Term
idTerm = Lam (intern "x") (Var (intern "x"))

idAssignment ∷ TypeAssignment
idAssignment = Map.fromList [(intern "x", SymT (intern "a"))]

churchTwo ∷ Term
churchTwo =
  Lam
    (intern "s")
    ( Lam
        (intern "z")
        ( App
            (Var (intern "s"))
            ( App
                (Var (intern "s"))
                (Var (intern "z"))
            )
        )
    )

churchThree ∷ Term
churchThree =
  Lam
    (intern "s")
    ( Lam
        (intern "z")
        ( App
            (Var (intern "s"))
            ( App
                (Var (intern "s"))
                ( App
                    (Var (intern "s"))
                    (Var (intern "z"))
                )
            )
        )
    )

churchAssignment ∷ TypeAssignment
churchAssignment =
  Map.fromList
    [ (intern "s", Pi Omega (SymT (intern "a")) (SymT (intern "a"))),
      (intern "z", SymT (intern "a"))
    ]

-- \y → ( (\n → n (\y → n (\_ → y))) (\x → (x (x y))) ) ∷ a → a
counterexample ∷ Term
counterexample =
  App
    ( Lam
        (intern "n")
        ( App
            (Var (intern "n"))
            ( Lam
                (intern "y")
                ( App
                    (Var (intern "n"))
                    ( Lam
                        (intern "z")
                        (Var (intern "y"))
                    )
                )
            )
        )
    )
    ( Lam
        (intern "x")
        ( App
            (Var (intern "x"))
            ( App
                (Var (intern "x"))
                (Var (intern "y"))
            )
        )
    )

arg0 ∷ Type
arg0 = SymT (intern "a")

arg1 ∷ Type
arg1 =
  Pi
    Omega
    (SymT (intern "a"))
    (SymT (intern "a"))

counterexampleAssignment ∷ Map.Map Symbol Type
counterexampleAssignment =
  Map.fromList
    [ (intern "n", Pi Omega arg1 arg0),
      (intern "y", arg0),
      (intern "z", arg0),
      (intern "x", arg1)
    ]

exp ∷ Term
exp =
  Lam
    (intern "m")
    ( Lam
        (intern "n")
        ( Lam
            (intern "s")
            ( Lam
                (intern "z")
                ( App
                    ( App
                        ( App
                            (Var (intern "m"))
                            (Var (intern "n"))
                        )
                        (Var (intern "s"))
                    )
                    (Var (intern "z"))
                )
            )
        )
    )

threeLam ∷ Term
threeLam = Lam (intern "f") (Lam (intern "x") (nTimesApp 10 (Var (intern "f")) (Var (intern "x"))))

threeLam2 ∷ Term
threeLam2 = Lam (intern "f'") (Lam (intern "x'") (nTimesApp 20 (Var (intern "f'")) (Var (intern "x'"))))

nTimesApp ∷ Int → Term → Term → Term
nTimesApp 0 _ b = b
nTimesApp n a b = App a (nTimesApp (n - 1) a b)

churchExp2 ∷ Term
churchExp2 = exp

churchExp ∷ Term
churchExp =
  Lam
    (intern "s'")
    ( Lam
        (intern "z'")
        ( App
            ( App
                ( App
                    ( App
                        exp
                        threeLam
                    )
                    threeLam2
                )
                (Var (intern "s'"))
            )
            (Var (intern "z'"))
        )
    )

zTy ∷ Type
zTy = SymT (intern "a")

sTy ∷ Type
sTy = Pi Omega zTy zTy

nat ∷ Type
nat = Pi Omega sTy sTy

churchExpAssignment ∷ TypeAssignment
churchExpAssignment =
  Map.fromList
    [ (intern "n", nat),
      (intern "m", Pi Omega nat nat),
      (intern "s", sTy),
      (intern "s'", sTy),
      (intern "z", zTy),
      (intern "z'", zTy),
      (intern "x'", zTy),
      (intern "x", sTy),
      (intern "f'", sTy),
      (intern "f", nat)
    ]
{- Examples from 3.0.1 of Asperti's book; they don't seem to typecheck though. -}

-- test1 = \x → \y → (\f → (\h → (h (\p → (h (\q → p)))) (\l → (((f (\n → (l n))) x) y))) (\g → \u → \v → ((g u) (g v))))

-- test2 = \x → \y → (\f → (\h → (h (\p → (h (\q → q)))) (\l → (((f (\n → (l n))) x) y))) (\g → \u → \v → ((g u) (g v))))

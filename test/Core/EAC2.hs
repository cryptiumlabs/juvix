module Core.EAC2 where

import qualified Juvix.Core.EAC as EAC
import Juvix.Core.EAC.Check
import Juvix.Core.Erased.Types hiding (Term, Type, TypeAssignment)
import qualified Juvix.Core.Erased.Types as ET
import qualified Juvix.Core.Types as Types
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Type, exp, link, reduce)
import qualified Juvix.Library.HashMap as Map
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

type Term = ET.Term ()

type Type = ET.Type ()

type TypeAssignment = ET.TypeAssignment ()

unitParam :: Types.Parameterisation () ()
unitParam =
  Types.Parameterisation
    { hasType = \_ ty -> ty == () :| [],
      arity = const 0,
      apply = \_ _ -> Nothing,
      builtinTypes = mempty,
      builtinValues = mempty,
      parseTy = const empty,
      parseVal = const empty,
      reservedNames = [],
      reservedOpNames = [],
      stringTy = \_ _ -> False,
      stringVal = const Nothing,
      intTy = \_ _ -> False,
      intVal = const Nothing,
      floatTy = \_ _ -> False,
      floatVal = const Nothing
    }

shouldGen ::
  T.TestName ->
  ( Either
      (EAC.Errors () ())
      (EAC.RPT (), EAC.ParamTypeAssignment ()) ->
    IO ()
  ) ->
  Types.TermAssignment () () () ->
  T.TestTree
shouldGen errorString case' termAssign =
  T.testCase (show (Types.term termAssign) <> errorString) $
    validEal unitParam termAssign >>= case'

shouldBeTypeable :: Types.TermAssignment () () () -> T.TestTree
shouldBeTypeable =
  shouldGen " should be typeable in EAC" $ \v ->
    case v of
      Right _ -> pure ()
      Left er -> T.assertFailure (show er)

shouldNotBeTypeable :: Types.TermAssignment () () () -> T.TestTree
shouldNotBeTypeable =
  shouldGen " should not be typeable in EAC" $ \v ->
    case v of
      Right _ -> T.assertFailure "a satisfying assignment was found"
      Left _ -> pure ()

eac2Tests :: T.TestTree
eac2Tests =
  T.testGroup
    "EAC2"
    []

--shouldBeTypeable idTerm idAssignment,
--shouldBeTypeable churchTwo churchAssignment,
--shouldBeTypeable churchThree churchAssignment,
--shouldNotBeTypeable counterexample counterexampleAssignment,
--shouldBeTypeable churchExp churchExpAssignment

idTerm :: Term
idTerm = Lam (intern "x") (Var (intern "x"))

idAssignment :: TypeAssignment
idAssignment = Map.fromList [(intern "x", SymT (intern "a"))]

churchTwo :: Term
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

churchThree :: Term
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

churchAssignment :: TypeAssignment
churchAssignment =
  Map.fromList
    [ (intern "s", Pi Usage.Omega (SymT (intern "a")) (SymT (intern "a"))),
      (intern "z", SymT (intern "a"))
    ]

-- \y → ( (\n → n (\y → n (\_ → y))) (\x → (x (x y))) ) ∷ a → a
counterexample :: Term
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

arg0 :: Type
arg0 = SymT (intern "a")

arg1 :: Type
arg1 =
  Pi
    Usage.Omega
    (SymT (intern "a"))
    (SymT (intern "a"))

counterexampleAssignment :: Map.Map Symbol Type
counterexampleAssignment =
  Map.fromList
    [ (intern "n", Pi Usage.Omega arg1 arg0),
      (intern "y", arg0),
      (intern "z", arg0),
      (intern "x", arg1)
    ]

exp :: Term
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

threeLam :: Term
threeLam = Lam (intern "f") (Lam (intern "x") (nTimesApp 10 (Var (intern "f")) (Var (intern "x"))))

threeLam2 :: Term
threeLam2 = Lam (intern "f'") (Lam (intern "x'") (nTimesApp 20 (Var (intern "f'")) (Var (intern "x'"))))

nTimesApp :: Int -> Term -> Term -> Term
nTimesApp 0 _ b = b
nTimesApp n a b = App a (nTimesApp (n - 1) a b)

churchExp2 :: Term
churchExp2 = exp

churchExp :: Term
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

zTy :: Type
zTy = SymT (intern "a")

sTy :: Type
sTy = Pi Usage.Omega zTy zTy

nat :: Type
nat = Pi Usage.Omega sTy sTy

churchExpAssignment :: TypeAssignment
churchExpAssignment =
  Map.fromList
    [ (intern "n", nat),
      (intern "m", Pi Usage.Omega nat nat),
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

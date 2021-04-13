module EAC2 where

import qualified Juvix.Core.EAC as EAC
import Juvix.Core.EAC.Check
import Juvix.Core.Erased.Types hiding (Term, Type, TypeAssignment)
import qualified Juvix.Core.Erased.Types as ET
import qualified Juvix.Core.Types as Types
import Juvix.Library hiding (Type, exp, link, reduce)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.Usage as Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

type Term = ET.Term ()

type Type = ET.Type ()

type TypeAssignment = ET.TypeAssignment ()

unitParam :: Types.Parameterisation () ()
unitParam =
  Types.Parameterisation
    { hasType = \_ ty -> ty == () :| [],
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
idTerm = Lam "x" (Var "x")

idAssignment :: TypeAssignment
idAssignment = Map.fromList [("x", SymT "a")]

churchTwo :: Term
churchTwo =
  Lam
    "s"
    ( Lam
        "z"
        ( App
            (Var "s")
            ( App
                (Var "s")
                (Var "z")
            )
        )
    )

churchThree :: Term
churchThree =
  Lam
    "s"
    ( Lam
        "z"
        ( App
            (Var "s")
            ( App
                (Var "s")
                ( App
                    (Var "s")
                    (Var "z")
                )
            )
        )
    )

churchAssignment :: TypeAssignment
churchAssignment =
  Map.fromList
    [ ("s", Pi Usage.Omega (SymT "a") (SymT "a")),
      ("z", SymT "a")
    ]

-- \y → ( (\n → n (\y → n (\_ → y))) (\x → (x (x y))) ) ∷ a → a
counterexample :: Term
counterexample =
  App
    ( Lam
        "n"
        ( App
            (Var "n")
            ( Lam
                "y"
                ( App
                    (Var "n")
                    ( Lam
                        "z"
                        (Var "y")
                    )
                )
            )
        )
    )
    ( Lam
        "x"
        ( App
            (Var "x")
            ( App
                (Var "x")
                (Var "y")
            )
        )
    )

arg0 :: Type
arg0 = SymT "a"

arg1 :: Type
arg1 =
  Pi
    Usage.Omega
    (SymT "a")
    (SymT "a")

counterexampleAssignment :: Map.Map Symbol Type
counterexampleAssignment =
  Map.fromList
    [ ("n", Pi Usage.Omega arg1 arg0),
      ("y", arg0),
      ("z", arg0),
      ("x", arg1)
    ]

exp :: Term
exp =
  Lam
    "m"
    ( Lam
        "n"
        ( Lam
            "s"
            ( Lam
                "z"
                ( App
                    ( App
                        ( App
                            (Var "m")
                            (Var "n")
                        )
                        (Var "s")
                    )
                    (Var "z")
                )
            )
        )
    )

threeLam :: Term
threeLam = Lam "f" (Lam "x" (nTimesApp 10 (Var "f") (Var "x")))

threeLam2 :: Term
threeLam2 = Lam "f'" (Lam "x'" (nTimesApp 20 (Var "f'") (Var "x'")))

nTimesApp :: Int -> Term -> Term -> Term
nTimesApp 0 _ b = b
nTimesApp n a b = App a (nTimesApp (n - 1) a b)

churchExp2 :: Term
churchExp2 = exp

churchExp :: Term
churchExp =
  Lam
    "s'"
    ( Lam
        "z'"
        ( App
            ( App
                ( App
                    ( App
                        exp
                        threeLam
                    )
                    threeLam2
                )
                (Var "s'")
            )
            (Var "z'")
        )
    )

zTy :: Type
zTy = SymT "a"

sTy :: Type
sTy = Pi Usage.Omega zTy zTy

nat :: Type
nat = Pi Usage.Omega sTy sTy

churchExpAssignment :: TypeAssignment
churchExpAssignment =
  Map.fromList
    [ ("n", nat),
      ("m", Pi Usage.Omega nat nat),
      ("s", sTy),
      ("s'", sTy),
      ("z", zTy),
      ("z'", zTy),
      ("x'", zTy),
      ("x", sTy),
      ("f'", sTy),
      ("f", nat)
    ]

{- Examples from 3.0.1 of Asperti's book; they don't seem to typecheck though. -}

-- test1 = \x → \y → (\f → (\h → (h (\p → (h (\q → p)))) (\l → (((f (\n → (l n))) x) y))) (\g → \u → \v → ((g u) (g v))))

-- test2 = \x → \y → (\f → (\h → (h (\p → (h (\q → q)))) (\l → (((f (\n → (l n))) x) y))) (\g → \u → \v → ((g u) (g v))))

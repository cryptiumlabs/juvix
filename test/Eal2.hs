module Eal2 where

import           Juvix.EAL.Check
import           Juvix.EAL.Types
import           Juvix.Library    hiding (Type, exp, link, reduce)

import qualified Data.Map.Strict  as Map
import qualified Test.Tasty       as T
import qualified Test.Tasty.HUnit as T

test_id ∷ T.TestTree
test_id = shouldBeTypeable idTerm idAssignment

test_churchTwo ∷ T.TestTree
test_churchTwo = shouldBeTypeable churchTwo churchAssignment

test_churchThree ∷ T.TestTree
test_churchThree = shouldBeTypeable churchThree churchAssignment

test_counterexample ∷ T.TestTree
test_counterexample = shouldNotBeTypeable counterexample counterexampleAssignment

test_church_exp ∷ T.TestTree
test_church_exp = shouldNotBeTypeable churchExp churchExpAssignment

shouldBeTypeable ∷ Term → TypeAssignment → T.TestTree
shouldBeTypeable term assignment =
  T.testCase (show term <> " should be typeable in EAL") $ do
    valid <- validEal term assignment
    case valid of
      Right _ -> return ()
      Left er -> T.assertFailure (show er)

shouldNotBeTypeable ∷ Term → TypeAssignment → T.TestTree
shouldNotBeTypeable term assignment =
  T.testCase (show term <> " should not be typeable in EAL") $ do
    valid <- validEal term assignment
    case valid of
      Right _ -> T.assertFailure "a satisfying assignment was found"
      Left _  -> pure ()

idTerm ∷ Term
idTerm = Lam (someSymbolVal "x") (Var (someSymbolVal "x"))

idAssignment ∷ TypeAssignment
idAssignment = Map.fromList [ (someSymbolVal "x", SymT (someSymbolVal "a")) ]

churchTwo ∷ Term
churchTwo = Lam (someSymbolVal "s")
             (Lam (someSymbolVal "z")
               (App (Var (someSymbolVal "s"))
                    (App (Var (someSymbolVal "s"))
                         (Var (someSymbolVal "z")))))

churchThree ∷ Term
churchThree = Lam (someSymbolVal "s")
             (Lam (someSymbolVal "z")
               (App (Var (someSymbolVal "s"))
                    (App (Var (someSymbolVal "s"))
                         (App (Var (someSymbolVal "s"))
                              (Var (someSymbolVal "z"))))))

churchAssignment ∷ TypeAssignment
churchAssignment = Map.fromList [
  (someSymbolVal "s", ArrT (SymT (someSymbolVal "a")) (SymT (someSymbolVal "a"))),
  (someSymbolVal "z", SymT (someSymbolVal "a"))
  ]

-- \y -> ( (\n -> n (\y -> n (\_ -> y))) (\x -> (x (x y))) ) :: a -> a
counterexample ∷ Term
counterexample =
  App
    (Lam (someSymbolVal "n")
      (App (Var (someSymbolVal "n"))
           (Lam (someSymbolVal "y")
              (App (Var (someSymbolVal "n"))
                        (Lam (someSymbolVal "z")
                        (Var (someSymbolVal "y")))
            )
      )
    ))
    (Lam (someSymbolVal "x")
      (App (Var (someSymbolVal "x"))
           (App (Var (someSymbolVal "x"))
                (Var (someSymbolVal "y")))))

arg0 ∷ Type
arg0 = SymT (someSymbolVal "a")

arg1 ∷ Type
arg1 = ArrT (SymT (someSymbolVal "a"))
            (SymT (someSymbolVal "a"))

counterexampleAssignment ∷ Map SomeSymbol Type
counterexampleAssignment = Map.fromList
  [ (someSymbolVal "n", ArrT arg1 arg0)
  , (someSymbolVal "y", arg0)
  , (someSymbolVal "z", arg0)
  , (someSymbolVal "x", arg1)
  ]

exp ∷ Term
exp =
  (Lam (someSymbolVal "m")
   (Lam (someSymbolVal "n")
    (Lam (someSymbolVal "s")
     (Lam (someSymbolVal "z")
       (App (App (App (Var (someSymbolVal "m"))
                 (Var (someSymbolVal "n")))
            (Var (someSymbolVal "s")))
            (Var (someSymbolVal "z")))))))

threeLam ∷ Term
threeLam = Lam (someSymbolVal "f")
           (Lam (someSymbolVal "x")
             (App (Var (someSymbolVal "f"))
                  (App (Var (someSymbolVal "f"))
                  (App (Var (someSymbolVal "f"))
                       (Var (someSymbolVal "x"))))))

threeLam2 ∷ Term
threeLam2 = Lam (someSymbolVal "f'")
           (Lam (someSymbolVal "x'")
             (App (Var (someSymbolVal "f'"))
                  (App (Var (someSymbolVal "f'"))
                       (App (Var (someSymbolVal "f'"))
                            (Var (someSymbolVal "x'"))))))

churchExp ∷ Term
churchExp =
    (Lam (someSymbolVal "s'")
      (Lam (someSymbolVal "z'")
        (App (App (App (App exp
                            threeLam)
                       threeLam2)
                  (Var (someSymbolVal "s'")))
             (Var (someSymbolVal "z'")))))

zTy ∷ Type
zTy = SymT (someSymbolVal "a")

sTy ∷ Type
sTy = ArrT zTy zTy

nat ∷ Type
nat = ArrT sTy sTy

churchExpAssignment ∷ TypeAssignment
churchExpAssignment = Map.fromList
  [ (someSymbolVal "n", nat)
  , (someSymbolVal "m", ArrT nat nat)
  , (someSymbolVal "s", sTy)
  , (someSymbolVal "s'", sTy)
  , (someSymbolVal "z", zTy)
  , (someSymbolVal "z'", zTy)
  , (someSymbolVal "x'", zTy)
  , (someSymbolVal "x", sTy)
  , (someSymbolVal "f'", sTy)
  , (someSymbolVal "f", nat)
  ]

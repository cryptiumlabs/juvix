import           Juvix.Eal.Check
import           Juvix.Eal.Eal2
import           Juvix.Eal.Solve
import           Juvix.Eal.Types2
import           Juvix.Library    hiding (Type, link, reduce)

import qualified Data.Map.Strict  as Map

testGen ∷ (RPT, Env)
testGen = execWithAssignment testAssignment
        $ generateConstraints testTerm

omegaTestGen ∷ (RPT, Env)
omegaTestGen = execWithAssignment omegaAssignment
             $ generateConstraints omegaTerm

testGen' ∷ ((RPT, ParamTypeAssignment), Env)
testGen' = execWithAssignment testAssignment
         $ generateTypeAndConstraitns testTerm

checkAnswer ∷ IO (Either Errors (RPT, ParamTypeAssignment))
checkAnswer = validEal testTerm testAssignment

checkAnswer2 ∷ IO (Either Errors (RPT, ParamTypeAssignment))
checkAnswer2 = validEal testTerm2 testAssignment

-- TODO ∷ Currently errors as unificationconstraints errors
checkAnswerOmega ∷ IO (Either Errors (RPT, ParamTypeAssignment))
checkAnswerOmega = validEal omegaTerm omegaAssignment

checkAnswerChurch ∷ IO (Either Errors (RPT, ParamTypeAssignment))
checkAnswerChurch = validEal churchMult churchMultTyp

checkAnswerChurchTwo ∷ IO (Either Errors (RPT, ParamTypeAssignment))
checkAnswerChurchTwo = validEal churchMultTwo churchMultTyp

checkAnswerNotTypeable ∷ IO (Either Errors (RPT, ParamTypeAssignment))
checkAnswerNotTypeable = validEal notTypeableInEalL notTypeableInEalTyp

resAnswer ∷ IO (Maybe RPT)
resAnswer = do
  let (rtp,env) = testGen
  assignments    ← getConstraints (constraints env)
  return $ fmap (flip assignTerm rtp) assignments

--resAnswerTwo ∷ IO (Maybe RPT)
resAnswerTwo ∷ IO (Maybe (RPT, ParamTypeAssignment))
resAnswerTwo = do
  let ((rtp, typ),env) = execWithAssignment churchMultTyp
                       $ generateTypeAndConstraitns churchMultTwo
  assignments    ← getConstraints (constraints env)
  case assignments of
    Just x  → pure $ Just (assignTerm x rtp, assignType x typ)
    Nothing → pure Nothing


resMulti ∷ IO ()
resMulti = uncurry (flip (runMultipleConstraints 10))
         $ second constraints
         $ testGen

res ∷ IO ()
res = runConstraints
    $ constraints
    $ snd
    $ testGen

cons ∷ RPT
cons = fst testGen

-- Test term: \s . \z . s s z.
testTerm ∷ Term
testTerm = Lam (someSymbolVal "s")
             (Lam (someSymbolVal "z")
               (App (Var (someSymbolVal "s"))
                    (App (Var (someSymbolVal "s"))
                         (Var (someSymbolVal "z")))))

testTerm2 ∷ Term
testTerm2 = Lam (someSymbolVal "s")
             (Lam (someSymbolVal "z")
               (App (Var (someSymbolVal "s"))
                    (App (Var (someSymbolVal "s"))
                         (App (Var (someSymbolVal "s"))
                              (Var (someSymbolVal "z"))))))

omegaTerm ∷ Term
omegaTerm =
  App
  (Lam (someSymbolVal "x")
    (App (Var (someSymbolVal "x"))
         (Var (someSymbolVal "x"))))
  (Lam (someSymbolVal "x")
    (App (Var (someSymbolVal "x"))
         (Var (someSymbolVal "x"))))

omegaAssignment ∷ TypeAssignment
omegaAssignment = Map.fromList
  [ (someSymbolVal "x", ArrT (SymT (someSymbolVal "a")) (SymT (someSymbolVal "a")))
  ]


notTypeableInEal ∷ ((p → p) → ((p → p) → p) → p) → ((p → p) → p) → p
notTypeableInEal = (\n -> (n (\y -> (n (\_ -> y)) (\x -> (x (x y))))))

notTypeableInEalL ∷ Term
notTypeableInEalL =
  (Lam (someSymbolVal "n")
    (App (Var (someSymbolVal "n"))
         (Lam (someSymbolVal "y")
           (App (App (Var (someSymbolVal "n"))
                     (Lam (someSymbolVal "z")
                       (Var (someSymbolVal "y"))))
                (Lam (someSymbolVal "x")
                  (App (Var (someSymbolVal "x"))
                       (App (Var (someSymbolVal "x"))
                            (Var (someSymbolVal "y")))))))))

arg0 ∷ Type
arg0 = SymT (someSymbolVal "a")

arg1 ∷ Type
arg1 = ArrT (SymT (someSymbolVal "a"))
            (SymT (someSymbolVal "a"))

arg2 ∷ Type
arg2 = ArrT arg1 (SymT (someSymbolVal "a"))

arg3 ∷ Type
arg3 = ArrT arg2 arg2

notTypeableInEalTyp ∷ Map SomeSymbol Type
notTypeableInEalTyp = Map.fromList
  [ (someSymbolVal "n", ArrT arg1 (ArrT arg2 arg0))
  , (someSymbolVal "y", arg0)
  , (someSymbolVal "z", arg0)
  , (someSymbolVal "x", arg1)
  ]

mult ∷ Term
mult =
  (Lam (someSymbolVal "m")
   (Lam (someSymbolVal "n'")
    (Lam (someSymbolVal "s")
     (Lam (someSymbolVal "z")
       (App (App (Var (someSymbolVal "m"))
                 (App (Var (someSymbolVal "n'"))
                      (Var (someSymbolVal "s"))))
            (Var (someSymbolVal "z")))))))


two' ∷ (t → t) → t → t
two' f x = f (f x)

twoLam ∷ Term
twoLam = Lam (someSymbolVal "f")
           (Lam (someSymbolVal "x")
             (App (Var (someSymbolVal "f"))
                  (App (Var (someSymbolVal "f"))
                       (Var (someSymbolVal "x")))))

churchMultTwo ∷ Term
churchMultTwo =
    (Lam (someSymbolVal "s'")
      (Lam (someSymbolVal "z'")
        (App (App (App (App mult
                            twoLam)
                       twoLam)
                  (Var (someSymbolVal "s'")))
             (Var (someSymbolVal "z'")))))

churchMult ∷ Term
churchMult =
  (Lam (someSymbolVal "n")
    (Lam (someSymbolVal "s'")
      (Lam (someSymbolVal "z'")
        (App (App (App (App mult
                            (Var (someSymbolVal "n")))
                       (Var (someSymbolVal "n")))
                  (Var (someSymbolVal "s'")))
             (Var (someSymbolVal "z'"))))))

churchMultBrief ∷ Term
churchMultBrief =
  (Lam (someSymbolVal "n")
   (App (App (Var (someSymbolVal "mult"))
             (Var (someSymbolVal "n")))
        (Var (someSymbolVal "n"))))

a4 ∷ Type
a4 = (ArrT (ArrT (SymT (someSymbolVal "a"))
                 (SymT (someSymbolVal "a")))
           (ArrT (SymT (someSymbolVal "a"))
                 (SymT (someSymbolVal "a"))))

churchMultTyp ∷ TypeAssignment
churchMultTyp = Map.fromList
  [ (someSymbolVal "mult", ArrT a4 (ArrT a4 a4))
  , (someSymbolVal "n", a4)
  , (someSymbolVal "n'", a4)
  , (someSymbolVal "m", a4)
  , (someSymbolVal "s", (ArrT (SymT (someSymbolVal "a"))
                              (SymT (someSymbolVal "a"))))
  , (someSymbolVal "s'", (ArrT (SymT (someSymbolVal "a"))
                               (SymT (someSymbolVal "a"))))
  , (someSymbolVal "z", (SymT (someSymbolVal "a")))
  , (someSymbolVal "z'", (SymT (someSymbolVal "a")))
  , (someSymbolVal "x", (SymT (someSymbolVal "a")))
  , (someSymbolVal "f", (ArrT (SymT (someSymbolVal "a"))
                              (SymT (someSymbolVal "a"))))
  ]

-- Test assignment - s : a → a, z : a.
testAssignment ∷ TypeAssignment
testAssignment = Map.fromList [
  (someSymbolVal "s", ArrT (SymT (someSymbolVal "a")) (SymT (someSymbolVal "a"))),
  (someSymbolVal "z", SymT (someSymbolVal "a"))
  ]

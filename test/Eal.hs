module Eal where

import           Juvix.EAL.EAL
import           Juvix.EAL.Parser
import           Juvix.Library

import           Text.Parsec

parseTest1 ∷ Either ParseError Term
parseTest1 = parseEal "!! (λ x : a -o b. !-!- x)"

parseTest2 ∷ Either ParseError Term
parseTest2 = parseEal "!!((λ x : Forall. x) (λx : a -o b. (x y)))"

parseTest3 ∷ Either ParseError Term
parseTest3 = parseEal "!!(λ x : Forall. !-!-x λx : a -o b -o c. !-!-(x y))"

testConstraint ∷ Either ParseError (Term, ConstraintTermEnv)
testConstraint = execBracketState . boxConstraint <$> parseEal "λy : Forall. λ z : Forall. (y (y z))"

exampleBracket ∷ Eal
exampleBracket =
  Lambda (someSymbolVal "y") Forall
    (Bang 0 (Lambda (someSymbolVal "z") Forall
               (Bang 1 (App (Bang 0 (App (Bang (-1) (Term (someSymbolVal "y")))
                                         (Bang (-1) (Term (someSymbolVal "y")))))
                            (Bang (-1) (Term (someSymbolVal "z")))))))

exampleTypeOf ∷ Eal
exampleTypeOf =
  App
    (Bang 1
      (Lambda (someSymbolVal "y") (BangT 1 Forall)
        (Bang (-1) (Term (someSymbolVal "y")))))
    (Bang 1
      (Lambda (someSymbolVal "y") Forall
        (Bang (-1) (Term (someSymbolVal "y")))))



exampleBracketRun ∷ Either BracketErrors ()
exampleBracketRun = runBracketChecker exampleBracket

exampleRun ∷ (Either TypeErrors Types, Info)
exampleRun = runTypeOf exampleTypeOf

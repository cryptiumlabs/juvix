module EAC where

import Juvix.Core.EAC
import Juvix.Core.Parameterisations.Unit
import Juvix.Library
import Text.Parsec

parseTest1 ∷ Either ParseError (RPTO UnitVal)
parseTest1 = parseEal "!! (λ x : a -o b. !-!- x)"

parseTest2 ∷ Either ParseError (RPTO UnitVal)
parseTest2 = parseEal "!!((λ x : Forall. x) (λx : a -o b. (x y)))"

parseTest3 ∷ Either ParseError (RPTO UnitVal)
parseTest3 = parseEal "!!(λ x : Forall. !-!-x λx : a -o b -o c. !-!-(x y))"

exampleBracket ∷ RPTO UnitVal
exampleBracket =
  RBang
    0
    ( RLam
        (intern "y")
        ( RBang
            0
            ( RLam
                (intern "z")
                ( RBang
                    1
                    ( RApp
                        ( RBang
                            0
                            ( RApp
                                (RBang (- 1) (RVar (intern "y")))
                                (RBang (- 1) (RVar (intern "y")))
                            )
                        )
                        (RBang (- 1) (RVar (intern "z")))
                    )
                )
            )
        )
    )

exampleBracketRun ∷ Either BracketErrors ()
exampleBracketRun = bracketChecker exampleBracket

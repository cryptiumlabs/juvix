module EAC where

import Juvix.EAC
import Juvix.Library
import Text.Parsec

parseTest1 ∷ Either ParseError RPTO

parseTest1 = parseEal "!! (λ x : a -o b. !-!- x)"

parseTest2 ∷ Either ParseError RPTO

parseTest2 = parseEal "!!((λ x : Forall. x) (λx : a -o b. (x y)))"

parseTest3 ∷ Either ParseError RPTO

parseTest3 = parseEal "!!(λ x : Forall. !-!-x λx : a -o b -o c. !-!-(x y))"

exampleBracket ∷ RPTO

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
                                (RBang (-1) (RVar (intern "y")))
                                (RBang (-1) (RVar (intern "y")))
                            )
                        )
                        (RBang (-1) (RVar (intern "z")))
                    )
                )
            )
        )
    )

exampleBracketRun ∷ Either BracketErrors ()

exampleBracketRun = bracketChecker exampleBracket

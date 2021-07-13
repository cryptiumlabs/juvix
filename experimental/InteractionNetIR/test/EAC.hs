module EAC where

import Juvix.Core.EAC
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Library
import Text.Parsec

parseTest1 :: Either ParseError (RPTO Unit.Val)
parseTest1 = parseEal "!! (λ x : a -o b. !-!- x)"

parseTest2 :: Either ParseError (RPTO Unit.Val)
parseTest2 = parseEal "!!((λ x : Forall. x) (λx : a -o b. (x y)))"

parseTest3 :: Either ParseError (RPTO Unit.Val)
parseTest3 = parseEal "!!(λ x : Forall. !-!-x λx : a -o b -o c. !-!-(x y))"

exampleBracket :: RPTO Unit.Val
exampleBracket =
  RBang
    0
    ( RLam
        "y"
        ( RBang
            0
            ( RLam
                "z"
                ( RBang
                    1
                    ( RApp
                        ( RBang
                            0
                            ( RApp
                                (RBang (- 1) (RVar "y"))
                                (RBang (- 1) (RVar "y"))
                            )
                        )
                        (RBang (- 1) (RVar "z"))
                    )
                )
            )
        )
    )

exampleBracketRun :: Either BracketErrors ()
exampleBracketRun = bracketChecker exampleBracket

module Nets.Combinators where

import Data.Graph.Inductive
import Juvix.Interpreter.InteractionNet.Backends.Env
import Juvix.Interpreter.InteractionNet.Backends.Graph
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.Nets.Combinators
import Juvix.Library
import Juvix.Utility

-- Example Graphs --------------------------------------------------------------
commute1 ∷ Net Lang
commute1 =
  buildGr
    [ ([(Edge (2, Prim) (1, Prim), 2)], 1, Con, []),
      ([], 2, Dup, [])
    ]

commute2 ∷ Net Lang
commute2 =
  buildGr
    [ ([(Edge (2, Prim) (1, Prim), 2)], 1, Con, []),
      ([], 2, Era, [])
    ]

commute3 ∷ Net Lang
commute3 =
  buildGr
    [ ([(Edge (2, Prim) (1, Prim), 2)], 1, Dup, []),
      ([], 2, Era, [])
    ]

annihilate1 ∷ Net Lang
annihilate1 =
  buildGr
    [ ([(Edge (2, Prim) (1, Prim), 2)], 1, Con, []),
      ([], 2, Con, [])
    ]

annihilate2 ∷ Net Lang
annihilate2 =
  buildGr
    [ ([(Edge (2, Prim) (1, Prim), 2)], 1, Dup, []),
      ([], 2, Dup, [])
    ]

annihilate3 ∷ Net Lang
annihilate3 =
  buildGr
    [ ([(Edge (2, Prim) (1, Prim), 2)], 1, Era, []),
      ([], 2, Era, [])
    ]

nonTerminating ∷ FlipNet Lang
nonTerminating =
  Flip $
    buildGr
      [ ( [ (Edge (2, Prim) (1, Prim), 2),
            (Edge (2, Aux1) (1, Aux2), 2),
            (Edge (3, Prim) (1, Aux1), 3)
          ],
          1,
          Con,
          []
        ),
        ( [ (Edge (4, Prim) (2, Aux2), 4)
          ],
          2,
          Dup,
          []
        ),
        ([], 3, Era, []),
        ([], 4, Era, [])
      ]

-- Tests------------------------------------------------------------------------

-- TODO ∷ Write real tests
test1 ∷ InfoNet (FlipNet Lang)
test1 = runFlipNet (reduceAll 100) nonTerminating

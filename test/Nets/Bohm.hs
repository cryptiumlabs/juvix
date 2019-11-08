module Nets.Bohm where

import Data.Graph.Inductive
import Juvix.Interpreter.InteractionNet.Backends.Env
import Juvix.Interpreter.InteractionNet.Backends.Graph
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.Nets.Bohm
import Juvix.Utility

test2 ∷ InfoNet (FlipNet Lang)
test2 =
  runFlipNet
    (reduceAll 10)
    ( Flip
        ( mkGraph
            [(1, Auxiliary2 Or), (2, Primar (IntLit 2))]
            [(1, 2, (Edge (1, Prim) (2, Prim)))]
        )
    )

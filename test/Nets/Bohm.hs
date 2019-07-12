import           Data.Graph.Inductive
import           Juvix.Nets.Bohm
import           Juvix.Interaction
import           Juvix.NodeInterface

test2 :: InfoNet Lang
test2 = runNet (reduceAll 10) (mkGraph [(1, Eq'), (2, IntLit' 2)]
                                       [(1,2, (Edge (1, Prim) (2, Prim)))])

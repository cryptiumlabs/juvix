import           Data.Graph.Inductive

import           Juvix.Nets.Combinators
import           Juvix.Interaction

-- Example Graphs --------------------------------------------------------------
commute1 ∷ NetLang
commute1 = buildGr
           [( [ (Edge (2, Prim) (1, Prim), 2) ], 1, Con, [])
           ,([], 2, Dup, [])
           ]

commute2 ∷ NetLang
commute2 = buildGr
           [ ( [(Edge (2, Prim) (1, Prim), 2)], 1, Con, [] )
           , ( [], 2, Era, [])
           ]

commute3 ∷ NetLang
commute3 = buildGr
           [ ( [(Edge (2, Prim) (1, Prim), 2)], 1, Dup, [] )
           , ( [], 2, Era, [])
           ]

annihilate1 ∷ NetLang
annihilate1 = buildGr
           [ ( [ (Edge (2, Prim) (1, Prim), 2) ], 1, Con, [])
           , ([], 2, Con, [])
           ]

annihilate2 ∷ NetLang
annihilate2 = buildGr
           [ ( [ (Edge (2, Prim) (1, Prim), 2) ], 1, Dup, [])
           , ([], 2, Dup, [])
           ]

annihilate3 ∷ NetLang
annihilate3 = buildGr
           [ ( [ (Edge (2, Prim) (1, Prim), 2) ], 1, Era, [])
           , ([], 2, Era, [])
           ]

nonTerminating ∷ NetLang
nonTerminating = buildGr
           [ ( [ (Edge (2, Prim) (1, Prim), 2)
               , (Edge (2, Aux1) (1, Aux2), 2)
               , (Edge (3, Prim) (1, Aux1), 3)
               ], 1, Con, [])
           , ( [ (Edge (4, Prim) (2, Aux2), 4)
               ], 2, Dup, [])
           , ( [], 3, Era, [] )
           , ( [], 4, Era, [] )
           ]
-- Tests------------------------------------------------------------------------

-- TODO: Write real tests
test1 :: InfoNet Lang
test1 = runNet (reduceAll 100) nonTerminating

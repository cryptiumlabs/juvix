-- |
-- - A simple function for showing the graph as a proper net
-- - Really should be improved upon or removed

module Juvix.Visualize.Graph where

import Data.Graph.Inductive (Graph)
import Data.Graph.Inductive.Dot (fglToDot, showDot)
import Juvix.Library hiding (writeFile)
import Prelude (writeFile)

showNet ∷ (Show a, Show b, Graph gr) ⇒ FilePath → gr a b → IO ()
showNet name graph = writeFile name (showDot (fglToDot graph))

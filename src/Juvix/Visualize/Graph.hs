module Juvix.Visualize.Graph where

import Data.Graph.Inductive.Dot
import Protolude hiding (writeFile)
import Prelude (writeFile)

showNet name graph = writeFile name (showDot (fglToDot graph))

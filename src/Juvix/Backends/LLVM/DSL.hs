-- |
-- Serves as a mini DSL layer above LLVM
-- * What is included?
-- 1. _Relink_
--    - gives a declarative way to do a bunch of links and relinks
module Juvix.Backends.LLVM.DSL where

import Juvix.Library hiding (reduce)
import Prelude (error)

-- | Type for specifying how one wants to link nodes
-- inspired from the interpreter version.
data Relink node port
  = RelAuxiliary
      { node ∷ node,
        primary ∷ Maybe (REL node port),
        auxiliary1 ∷ Maybe (REL node port),
        auxiliary2 ∷ Maybe (REL node port),
        auxiliary3 ∷ Maybe (REL node port),
        auxiliary4 ∷ Maybe (REL node port)
      }

defRel ∷ Relink node port
defRel =
  RelAuxiliary
    (error "put in default node into relAuxiliary")
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- | REL: a type that displays whether we are linking from an old node or just adding a new link
data REL node port
  = Link node
  | ReLink node port
  deriving (Show)

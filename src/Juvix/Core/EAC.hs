-- |
-- - This directory contains the implementation of =Eal= and the inferred
--   bracket checker on =Eal=
module Juvix.Core.EAC
  ( module Juvix.Core.EAC.ConstraintGen,
    module Juvix.Core.EAC.Types,
    module Juvix.Core.EAC.Erasure,
    module Juvix.Core.EAC.Parser,
    module Juvix.Core.EAC.Check,
    module Juvix.Core.EAC.Solve,
  )
where

import Juvix.Core.EAC.Check
import Juvix.Core.EAC.ConstraintGen
import Juvix.Core.EAC.Erasure
import Juvix.Core.EAC.Parser
import Juvix.Core.EAC.Solve
import Juvix.Core.EAC.Types

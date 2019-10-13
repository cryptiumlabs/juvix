module Erasure where

import qualified Juvix.Backends.Env as Env
import qualified Juvix.Backends.Graph as Graph
import qualified Juvix.Backends.Maps as Maps ()
import qualified Juvix.Bohm as Bohm
import qualified Juvix.Core as Core
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.EAC as EAC
import Juvix.Library hiding (identity)
import qualified Juvix.Nets.Bohm as Bohm
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

eraseSolveEval :: Core.CTerm -> IO ()
eraseSolveEval cterm = do
  let (term, typeAssignment) = Erasure.erase' cterm
  res <- EAC.validEal term typeAssignment
  putText ("Inferred EAC term & type: " <> show res :: Text)
  case res of
    Left _ -> return ()
    Right (term, assigmnent) -> do
      let bohm = EAC.ealToBohm term
      putText ("Converted to BOHM: " <> show bohm :: Text)
      let net :: Graph.FlipNet Bohm.Lang
          net = Bohm.astToNet bohm Bohm.defaultEnv
      putText ("Translated to net: " <> show net :: Text)
      let reduced = Graph.runFlipNet (Bohm.reduceAll 1000000) net
          info = Env.info reduced
          res = Env.net reduced
      putText ("Reduced net: " <> show res :: Text)
      let readback = Bohm.netToAst res
      putText ("Reduction info: " <> show info :: Text)
      putText ("Read-back term: " <> show readback :: Text)

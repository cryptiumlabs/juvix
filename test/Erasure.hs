module Erasure where

import qualified Juvix.Backends.Env   as Env
import qualified Juvix.Backends.Graph as Graph
import qualified Juvix.Backends.Maps  as Maps ()
import qualified Juvix.Bohm           as Bohm
import qualified Juvix.Core           as Core
import qualified Juvix.Core.Erasure   as Erasure
import qualified Juvix.EAC            as EAC
import           Juvix.Library        hiding (identity)
import qualified Juvix.Nets.Bohm      as Bohm

import qualified Test.Tasty           as T
import qualified Test.Tasty.HUnit     as T

identity ∷ Core.CTerm
identity = Core.Lam (Core.Conv (Core.Bound 0))

identityCompTy ∷ Core.Annotation
identityCompTy = (Core.SNat 1, Core.VPi (Core.SNat 1) Core.VNats (const (Core.VNats)))

identityContTy ∷ Core.Annotation
identityContTy = (Core.SNat 0, Core.VPi (Core.SNat 0) Core.VNats (const (Core.VNats)))

test_identity_computational ∷ T.TestTree
test_identity_computational = shouldCheck identity identityCompTy

test_identity_contemplation ∷ T.TestTree
test_identity_contemplation = shouldCheck identity identityContTy

shouldCheck ∷ Core.CTerm → Core.Annotation → T.TestTree
shouldCheck term ann =
  T.testCase (show term <> " should check as type " <> show ann) $
    Core.cType 0 [] term ann T.@=? Right ()

shouldInfer ∷ Core.ITerm → Core.Annotation → T.TestTree
shouldInfer term ann =
  T.testCase (show term <> " should infer to type " <> show ann) $
    Core.iType0 [] term T.@=? Right ann

one ∷ Core.CTerm
one = Core.Lam (Core.Lam (Core.Conv (Core.App (Core.Bound 1) (Core.Conv (Core.Bound 0)))))

oneCompTy ∷ Core.Annotation
oneCompTy = (Core.SNat 1, Core.VPi (Core.SNat 1) (Core.VPi (Core.SNat 1) Core.VNats (const Core.VNats)) (const (Core.VPi (Core.SNat 1) Core.VNats (const Core.VNats))))

two ∷ Core.CTerm
two = Core.Lam (Core.Lam (Core.Conv (Core.App (Core.Bound 1) (Core.Conv (Core.App (Core.Bound 1) (Core.Conv (Core.Bound 0)))))))

twoCompTy ∷ Core.Annotation
twoCompTy = (Core.SNat 1, Core.VPi (Core.SNat 2) (Core.VPi (Core.SNat 1) Core.VNats (const Core.VNats)) (const (Core.VPi (Core.SNat 1) Core.VNats (const Core.VNats))))

eraseSolveEval ∷ Core.CTerm → IO ()
eraseSolveEval cterm = do
  let (term, typeAssignment) = Erasure.erase' cterm
  res <- EAC.validEal term typeAssignment
  putText ("Inferred EAC term & type: " <> show res :: Text)
  case res of
    Left _ -> return ()
    Right (term, assigmnent) -> do
      let bohm = EAC.ealToBohm term
      putText ("Converted to BOHM: " <> show bohm :: Text)
      let net ∷ Graph.FlipNet Bohm.Lang
          net = Bohm.astToNet bohm Bohm.defaultEnv
      putText ("Translated to net: " <> show net :: Text)
      let reduced = Graph.runFlipNet (Bohm.reduceAll 1000000) net
          info = Env.info reduced
          res = Env.net reduced
      putText ("Reduced net: " <> show res :: Text)
      let readback = Bohm.netToAst res
      putText ("Reduction info: " <> show info :: Text)
      putText ("Read-back term: " <> show readback :: Text)

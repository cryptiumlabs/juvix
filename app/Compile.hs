module Compile where

import Config
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import Juvix.Backends.Michelson.Parameterisation
import qualified Juvix.Core as Core
import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.HR as Core
import Juvix.Library
import Options
import Types

typecheck ∷ FilePath → Backend → IO (Erased.Term PrimVal, Erased.Type PrimTy)
typecheck fin Michelson = do
  source ← readFile fin
  let parsed = Core.generateParser michelson (T.unpack source)
  case parsed of
    Just (HR.Elim (HR.Ann usage term ty)) → do
      erased ← liftIO (exec (Core.typecheckErase term usage ty) michelson)
      case erased of
        (Right ((term, ty), typeAssignment), _) →
          pure (term, ty)
        other → do
          T.putStrLn (show other)
          exitFailure
    err → do
      T.putStrLn (show err)
      exitFailure
typecheck _ _ = exitFailure

compile ∷ FilePath → FilePath → Backend → IO ()
compile fin fout backend = do
  (term, ty) ← typecheck fin backend
  -- TODO: Annotated version.
  let (res, logs) = M.compile undefined undefined
  case res of
    Left err → do
      T.putStrLn (show err)
      exitFailure
    Right c → do
      T.writeFile fout (M.contractToSource c)
compile _ _ _ = exitFailure

module Compile where

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core as Core
import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.HR as Core
import Juvix.Library
import Options
import Types

typecheck ∷
  FilePath → Backend → IO (Erased.Term Param.PrimVal, Erased.Type Param.PrimTy)
typecheck fin Michelson = do
  source ← readFile fin
  let parsed = Core.generateParser Param.michelson (T.unpack source)
  case parsed of
    Just (HR.Elim (HR.Ann usage term ty)) → do
      erased ← liftIO (exec (Core.typecheckErase term usage ty) Param.michelson)
      case erased of
        (Right ((term, ty), _typeAssignment), _) →
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
  (_term, _ty) ← typecheck fin backend
  -- TODO: Annotated version.
  let (res, _logs) = M.compileContract undefined undefined
  case res of
    Left err → do
      T.putStrLn (show err)
      exitFailure
    Right c → do
      T.writeFile fout (M.untypedContractToSource (fst c))
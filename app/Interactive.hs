module Interactive where

import qualified Config
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Juvix.Core as Core
import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.Parameterisations.Naturals as Nat
-- import qualified Juvix.Interpreter.InteractionNet as INet
-- import qualified Juvix.Interpreter.InteractionNet.Backends.Env as Env
-- import qualified Juvix.Interpreter.InteractionNet.Backends.Graph as Graph
-- import qualified Juvix.Interpreter.InteractionNet.Nets.Default as INet
import Juvix.Library
import Options
import qualified System.Console.Haskeline as H
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import Types
import Prelude (String)

interactive :: Context -> Config.T -> IO ()
interactive ctx _ = do
  let func = return
  H.runInputT (settings ctx) (mainLoop func)

settings :: Context -> H.Settings IO
settings ctx =
  H.Settings
    { H.complete = H.completeFilename,
      H.historyFile = Just (contextHomeDirectory ctx <> "/.jvxi_history"),
      H.autoAddHistory = True
    }

mainLoop :: (String -> IO String) -> H.InputT IO ()
mainLoop func = do
  input <- H.getInputLine "jvxi >> "
  case input of
    Nothing -> return ()
    Just i ->
      case i of
        (':' : special) -> handleSpecial special (mainLoop func)
        inp -> do
          H.outputStrLn =<< liftIO (func inp)
          mainLoop func

parseString :: String -> Maybe (HR.Term Nat.Ty Nat.Val)
parseString = HR.generateParser Nat.t

handleSpecial ::
  String -> H.InputT IO () -> H.InputT IO ()
handleSpecial str cont =
  case str of
    "?" -> liftIO (putDoc specialsDoc) >> cont
    "exit" -> return ()
    "tutorial" -> do
      H.outputStrLn "Interactive tutorial coming soon!"
      cont
    'c' : 'p' : ' ' : rest -> do
      let parsed = parseString rest
      H.outputStrLn (show parsed)
      cont
    'c' : 'e' : ' ' : rest -> do
      let parsed = parseString rest
      H.outputStrLn (show parsed)
      case parsed of
        Just (HR.Elim (HR.Ann usage term ty _)) -> do
          let makeErased = fst <$> Core.typecheckErase' term usage ty
          erased <-
            liftIO $
              (exec makeErased Nat.t mempty :: Exec Nat.Ty Nat.Val ())
          H.outputStrLn (show erased)
        _ -> H.outputStrLn "must enter a valid annotated core term"
      cont
    'c' : 't' : ' ' : _rest -> do
      H.outputStrLn "TODO fix typecheckAffineErase"
      -- let parsed = parseString rest
      -- H.outputStrLn (show parsed)
      -- case parsed of
      --   Just (HR.Elim (HR.Ann usage term ty _)) -> do
      --     erased <-
      --       liftIO
      --         ( exec (Core.typecheckAffineErase term usage ty) Nat.t mempty ::
      --             ExecTerm Nat.Ty Nat.Val ()
      --         )
      --     H.outputStrLn (show erased)
      --     case erased of
      --       (Right (Core.Assignment term _), _) ->
      --         transformAndEvaluateErasedCore Nat.t True term
      --       _ -> return ()
      --   _ -> H.outputStrLn "must enter a valid annotated core term"
      cont
    _ -> H.outputStrLn "Unknown special command" >> cont

-- transformAndEvaluateErasedCore ::
--   forall primTy primVal.
--   ( Show primVal,
--     Core.CanApply primVal
--   ) =>
--   Core.Parameterisation primTy primVal ->
--   Bool ->
--   Erased.Term primVal ->
--   H.InputT IO ()
-- transformAndEvaluateErasedCore parameterisation debug term = do
--   let ast = INet.erasedCoreToInteractionNetAST term
--   when debug $ H.outputStrLn ("Converted to AST: " <> show ast)
--   let net :: Graph.FlipNet (INet.Lang primVal)
--       net = INet.astToNet parameterisation ast INet.defaultEnv
--   when debug $ H.outputStrLn ("Translated to net: " <> show net)
--   let reduced = Graph.runFlipNet (INet.reduceAll 1000000) net
--       info = Env.info reduced
--       res = Env.net reduced
--   when debug $ H.outputStrLn ("Reduced net: " <> show res)
--   let readback = INet.netToAst res
--   when debug $ H.outputStrLn ("Reduction info: " <> show info)
--   H.outputStrLn ("Read-back term: " <> show readback)

specialsDoc :: Doc
specialsDoc =
  mconcat [line, mconcat (fmap (flip (<>) line . specialDoc) specials), line]

specialDoc :: Special -> Doc
specialDoc (Special command helpDesc) =
  text $ T.unpack $ mconcat [":", command, " - ", helpDesc]

specials :: [Special]
specials =
  [ Special "cp [term]" "Parse a core term",
    Special "ce [term]" "Parse, typecheck, & erase a core term",
    Special "ct [term}" "Parse, typecheck, & evaluate a core term",
    Special
      "cs [term"
      ( "Parse a core term, erase it, "
          <> "translate it to EAC, solve constraints, evaluate & read-back"
      ),
    Special "tutorial" "Embark upon an interactive tutorial",
    Special "?" "Show this help message",
    Special "exit" "Quit interactive mode"
  ]

data Special
  = Special
      { specialCommand :: Text,
        specialHelpDesc :: Text
      }

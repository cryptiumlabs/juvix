module Interactive where

import Config
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Juvix.Backends.Env as Env
import qualified Juvix.Backends.Graph as Graph
import qualified Juvix.Backends.Maps as Maps ()
import qualified Juvix.Bohm as Bohm
import qualified Juvix.Core as Core
import qualified Juvix.EAC as EAC
import qualified Juvix.Nets.Bohm as Bohm
import Options
import Protolude
import qualified System.Console.Haskeline as H
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import Prelude (String)

interactive ∷ Context → Config → IO ()
interactive ctx _ = do
  func ← return $ \str → return str
  H.runInputT (settings ctx) (mainLoop func)

settings ∷ Context → H.Settings IO
settings ctx =
  H.Settings
    { H.complete = H.completeFilename,
      H.historyFile = Just (contextHomeDirectory ctx <> "/.jvxi_history"),
      H.autoAddHistory = True
    }

mainLoop ∷ (String → IO String) → H.InputT IO ()
mainLoop func = do
  input ← H.getInputLine "jvxi >> "
  case input of
    Nothing → return ()
    Just i → do
      case i of
        (':' : special) → handleSpecial special (mainLoop func)
        inp → do
          H.outputStrLn =<< liftIO (func inp)
          mainLoop func

handleSpecial ∷ String → H.InputT IO () → H.InputT IO ()
handleSpecial str cont = do
  case str of
    "?" → liftIO (putDoc specialsDoc) >> cont
    "exit" → return ()
    "tutorial" → do
      H.outputStrLn "Interactive tutorial coming soon!"
      cont
    'c' : 'p' : ' ' : rest → do
      let parsed = Core.parseString Core.cterm rest
      H.outputStrLn $ show parsed
      cont
    'c' : 't' : ' ' : rest → do
      let parsed = Core.parseString Core.cterm rest
      H.outputStrLn $ show parsed
      case parsed of
        Just cterm → do
          let eval = Core.cEval cterm []
          H.outputStrLn $ show eval
        Nothing → return ()
      cont
    'c' : 'e' : ' ' : rest → do
      let parsed = Core.parseString Core.cterm rest
      H.outputStrLn $ show parsed
      case parsed of
        Just cterm → do
          eal ← eraseAndSolveCore cterm
          case eal of
            Right (term, _) → do
              transformAndEvaluateEal True term
            _ → return ()
        Nothing → return ()
      cont
    'e' : 'p' : ' ' : rest → do
      let parsed = EAC.parseEal rest
      case parsed of
        Right r → transformAndEvaluateEal True r
        _ → return ()
      cont
    'e' : 'q' : ' ' : rest → do
      let parsed = EAC.parseEal rest
      case parsed of
        Right r → transformAndEvaluateEal False r
        _ → return ()
      cont
    'e' : 'e' : ' ' : rest → do
      let parsed = EAC.parseEal rest
      H.outputStrLn $ show parsed
      case parsed of
        Right r → transformAndEvaluateEal True r
        _ → return ()
      cont
    _ → H.outputStrLn "Unknown special command" >> cont

eraseAndSolveCore ∷
  Core.CTerm → H.InputT IO (Either EAC.Errors (EAC.RPT, EAC.ParamTypeAssignment))
eraseAndSolveCore cterm = do
  let (term, typeAssignment) = Core.erase' cterm
  res ← liftIO (EAC.validEal term typeAssignment)
  H.outputStrLn ("Inferred EAC term & type: " <> show res)
  pure res

transformAndEvaluateEal ∷ Bool → EAC.RPTO → H.InputT IO ()
transformAndEvaluateEal debug term = do
  let bohm = EAC.ealToBohm term
  when debug $ H.outputStrLn ("Converted to BOHM: " <> show bohm)
  let net ∷ Graph.FlipNet Bohm.Lang
      net = Bohm.astToNet bohm Bohm.defaultEnv
  when debug $ H.outputStrLn ("Translated to net: " <> show net)
  let reduced = Graph.runFlipNet (Bohm.reduceAll 1000000) net
      info = Env.info reduced
      res = Env.net reduced
  when debug $ H.outputStrLn ("Reduced net: " <> show res)
  let readback = Bohm.netToAst res
  when debug $ H.outputStrLn ("Reduction info: " <> show info)
  H.outputStrLn ("Read-back term: " <> show readback)

specialsDoc ∷ Doc
specialsDoc =
  mconcat
    [ line,
      mconcat (fmap (flip (<>) line . specialDoc) specials),
      line
    ]

specialDoc ∷ Special → Doc
specialDoc (Special command helpDesc) =
  text $ T.unpack $ mconcat [":", command, " - ", helpDesc]

specials ∷ [Special]
specials =
  [ Special "cp [term]" "Parse a Juvix Core term",
    Special "ct [term}" "Parse, typecheck, & evaluate a Juvix Core term",
    Special
      "ce [term"
      "Parse a Juvix Core term, translate to EAC, solve constraints, evaluate & read-back",
    Special "ep [term]" "Parse an EAC term",
    Special "ee [term]" "Parse an EAC term, evaluate & read-back",
    Special "eq [term]" "Parse an EAC term, evaluate & read-back quietly",
    Special "tutorial" "Embark upon an interactive tutorial",
    Special "?" "Show this help message",
    Special "exit" "Quit interactive mode"
  ]

data Special
  = Special
      { specialCommand ∷ Text,
        specialHelpDesc ∷ Text
      }

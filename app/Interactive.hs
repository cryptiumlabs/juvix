module Interactive where

import Config
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Juvix.Backends.Env as Env
import qualified Juvix.Backends.Graph as Graph
import qualified Juvix.Backends.Maps as Maps ()
import qualified Juvix.Bohm as Bohm
import qualified Juvix.Core as Core
import qualified Juvix.Core.Erased as Erased
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.HR as Core
import Juvix.Core.Parameterisations.Naturals
import Juvix.Library
import qualified Juvix.Nets.Bohm as Bohm
import Monad
import Options
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

parseString ∷ String → Maybe (Core.Term NatTy NatVal)
parseString = Core.generateParser nat

handleSpecial ∷ String → H.InputT IO () → H.InputT IO ()
handleSpecial str cont = do
  case str of
    "?" → liftIO (putDoc specialsDoc) >> cont
    "exit" → return ()
    "tutorial" → do
      H.outputStrLn "Interactive tutorial coming soon!"
      cont
    'c' : 'p' : ' ' : rest → do
      let parsed = parseString rest
      H.outputStrLn (show parsed)
      cont
    'c' : 'e' : ' ' : rest → do
      let parsed = parseString rest
      H.outputStrLn (show parsed)
      case parsed of
        Just (HR.Elim (HR.Ann usage term ty)) → do
          erased ← liftIO (exec (Core.typecheckErase term usage ty))
          H.outputStrLn (show erased)
        _ → H.outputStrLn "must enter a valid annotated core term"
      cont
    'c' : 't' : ' ' : rest → do
      let parsed = parseString rest
      H.outputStrLn (show parsed)
      case parsed of
        Just (HR.Elim (HR.Ann usage term ty)) → do
          erased ← liftIO (exec (Core.typecheckAffineErase term usage ty))
          H.outputStrLn (show erased)
          case erased of
            (Right (term, _), _) → do
              transformAndEvaluateErasedCore True term
            _ → return ()
        _ → H.outputStrLn "must enter a valid annotated core term"
      cont
    _ → H.outputStrLn "Unknown special command" >> cont

transformAndEvaluateErasedCore ∷ ∀ primVal. Bool → Erased.Term primVal → H.InputT IO ()
transformAndEvaluateErasedCore debug term = do
  let bohm = Bohm.erasedCoreToBohm term
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
  mconcat [line, mconcat (fmap (flip (<>) line . specialDoc) specials), line]

specialDoc ∷ Special → Doc
specialDoc (Special command helpDesc) =
  text $ T.unpack $ mconcat [":", command, " - ", helpDesc]

specials ∷ [Special]
specials =
  [ Special "cp [term]" "Parse a core term",
    Special "ce [term]" "Parse, typecheck, & erase a core term",
    Special "ct [term}" "Parse, typecheck, & evaluate a core term",
    Special
      "cs [term"
      "Parse a core term, erase it, translate it to EAC, solve constraints, evaluate & read-back",
    Special "tutorial" "Embark upon an interactive tutorial",
    Special "?" "Show this help message",
    Special "exit" "Quit interactive mode"
  ]

data Special
  = Special
      { specialCommand ∷ Text,
        specialHelpDesc ∷ Text
      }

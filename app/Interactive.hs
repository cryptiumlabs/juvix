module Interactive where

import           Control.Monad.IO.Class
import qualified Data.Text                    as T
import           Prelude                      (String)
import           Protolude
import qualified System.Console.Haskeline     as H
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           Config
import           Options

import qualified Juvix.Backends.Env           as Env
import qualified Juvix.Backends.Maps          as Maps
import qualified Juvix.Bohm                   as Bohm
import qualified Juvix.Core                   as Core
import qualified Juvix.EAL                    as EAL
import qualified Juvix.Nets.Bohm              as Bohm

interactive ∷ Context → Config → IO ()
interactive ctx _ = do
  func ← return $ \str -> do
    return str
  H.runInputT (settings ctx) (mainLoop func)

settings ∷ Context → H.Settings IO
settings ctx = H.Settings {
  H.complete        = H.completeFilename,
  H.historyFile     = Just (contextHomeDirectory ctx <> "/.jvxi_history"),
  H.autoAddHistory  = True
  }

mainLoop ∷ (String → IO String) → H.InputT IO ()
mainLoop func = do
  input ← H.getInputLine "jvxi >> "
  case input of
    Nothing → return ()
    Just i  → do
      case i of
        (':' : special) →
          handleSpecial special (mainLoop func)
        inp → do
          H.outputStrLn =<< liftIO (func inp)
          mainLoop func

handleSpecial ∷ String → H.InputT IO () → H.InputT IO ()
handleSpecial str cont = do
  case str of
    "?"    → liftIO (putDoc specialsDoc) >> cont
    "exit" → return ()
    "tutorial" → do
      H.outputStrLn "Interactive tutorial coming soon!"
      cont
    'c' : ' ' : rest -> do
      let parsed = Core.parseString Core.cterm rest
      H.outputStrLn $ show parsed
      cont
    'e' : ' ' : rest -> do
      let parsed = EAL.parseEal rest
      H.outputStrLn $ show parsed
      case parsed of
        Right r -> transformAndEvaluateEal r
        _       -> return ()
      cont
    _      → H.outputStrLn "Unknown special command" >> cont

transformAndEvaluateEal ∷ EAL.RPTO → H.InputT IO ()
transformAndEvaluateEal term = do
  let bohm = EAL.ealToBohm term
  H.outputStrLn ("Converted to BOHM: " <> show bohm)
  let net ∷ Maps.Net Bohm.Lang
      net = Bohm.astToNet bohm
  H.outputStrLn ("Translated to net: " <> show net)
  let reduced = Maps.runMapNet (Bohm.reduceAll 1000000) net
      info = Env.info reduced
      res = Env.net reduced
      readback = Bohm.netToAst res
  H.outputStrLn ("Reduction info: " <> show info)
  H.outputStrLn ("Read-back term: " <> show readback)

specialsDoc ∷ Doc
specialsDoc = mconcat [
  line,
  mconcat (fmap (flip (<>) line . specialDoc) specials),
  line
  ]

specialDoc ∷ Special → Doc
specialDoc (Special command helpDesc) = text $ T.unpack $ mconcat [":", command, " - ", helpDesc]

specials ∷ [Special]
specials = [
  Special "c [term]"  "Parse a Juvix Core term",
  Special "e [term]"  "Parse an EAL term",
  Special "tutorial"  "Embark upon an interactive tutorial",
  Special "?"         "Show this help message",
  Special "exit"      "Quit interactive mode"
  ]

data Special = Special {
  specialCommand  ∷ Text,
  specialHelpDesc ∷ Text
}

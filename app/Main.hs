module Main where

import qualified Compile as Compile
import qualified Config as Config
import qualified Interactive as Interactive
import Options
import Options.Applicative
import Protolude
import ReplDoc
import System.Directory
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import Text.RawString.QQ

context ∷ IO Context
context = do
  pwd ← getCurrentDirectory
  home ← getHomeDirectory
  return (Context pwd home)

main ∷ IO ()
main = do
  ctx ← context
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run ctx =<< execParser opts

interactiveDoc ∷ Doc
interactiveDoc =
  mconcat
    [ aboutDoc,
      line,
      white
        [r|
     | \ \   / /\ \/ (_)
  _  | |\ \ / /  \  /| |
 | |_| | \ V /   /  \| |
  \___/   \_/   /_/\_\_|
|],
      mconcat
        [ line,
          "Juvix interactive alpha.",
          line,
          "Currently supported backends: "
            <> "in-process interpreter, in-process interaction net.",
          line,
          "Coming soon: Michelson, LLVM, WASM.",
          line,
          "Enter :? for help. Enter :tutorial for an interactive tutorial.",
          line
        ]
    ]

run ∷ Context → Options → IO ()
run ctx (Options cmd configPath) = do
  maybeConfig ← Config.loadT configPath
  let conf = fromMaybe Config.defaultT maybeConfig
  case cmd of
    Typecheck fin backend → do
      Compile.typecheck fin backend >> pure ()
    Compile fin fout backend →
      Compile.compile fin fout backend
    Interactive → do
      putDoc interactiveDoc
      if isJust maybeConfig
        then putStrLn ("Loaded runtime configuration from " <> configPath <> "\n")
        else putStrLn ("Loaded default runtime configuration.\n" ∷ Text)
      Interactive.interactive ctx conf
      exitSuccess
    Version → do
      putDoc versionDoc
      exitSuccess
    _ → do
      putText "Not yet implemented!"
      exitFailure

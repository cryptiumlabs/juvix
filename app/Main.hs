module Main
  ( main,
  )
where

import qualified Data.Aeson as A
import Data.Curve.Weierstrass.BLS12381 (Fr)
import Data.Field.Galois (Prime, fromP, toP)
import qualified Data.Scientific as S
import Development.GitRev
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Pipeline (BMichelson, BPlonk)
import qualified Juvix.Pipeline as Pipeline
import Options
import Options.Applicative
import System.Directory
import Text.Pretty.Simple (pPrint)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import Text.RawString.QQ

instance A.FromJSON Fr where
  parseJSON (A.Number n) = case S.floatingOrInteger n of
    Left floating -> panic $ "Can't parse floating :" <> show n
    Right f -> pure . toP $ toInteger f

instance A.ToJSON Fr where
  toJSON f = A.Number $ S.scientific (fromP f) 0

context :: IO Context
context = do
  pwd <- getCurrentDirectory
  home <- getHomeDirectory
  return (Context pwd home)

main :: IO ()
main = do
  ctx <- context
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run ctx =<< execParser opts

disclaimerDoc :: Doc
disclaimerDoc =
  mconcat
    [ "This is ",
      red "experimental",
      " software released for research purposes only – use at your own risk.",
      line,
      "Juvix may diverge from canonical"
        <> "protocol implementations in unexpected ways."
    ]

aboutDoc :: Doc
aboutDoc =
  mconcat
    [ text
        ( "Juvix smart contract language compiler,"
            <> "debugging toolkit, & stateful deployment system"
        ),
      line,
      text
        ( "(c) Christopher Goes 2018-2019, "
            <> "(c) Cryptium Labs / Metastate 2019-2020 • https://juvix.org"
        ),
      line,
      disclaimerDoc
    ]

versionDoc :: Doc
versionDoc =
  mconcat
    [ aboutDoc,
      line <> line,
      mconcat ["Prerelease version.", line],
      mconcat
        [ "Built from branch ",
          white $(gitBranch),
          " at commit ",
          magenta $(gitHash),
          " (commit date ",
          cyan $(gitCommitDate),
          ").",
          line
        ]
    ]

interactiveDoc :: Doc
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

-- | Run the main program.
run :: Context -> Options -> IO ()
run ctx opt = do
  feedback <- Feedback.runFeedbackT $ run' ctx opt
  case feedback of
    Feedback.Success msgs _ -> mapM_ pPrint msgs >> exitSuccess
    Feedback.Fail msgs -> mapM_ pPrint msgs >> exitFailure
  where
    run' :: Context -> Options -> Pipeline.Pipeline ()
    run' _ (Options cmd _) = do
      case cmd of
        Parse fin backend -> runCmd fin backend Pipeline.parse
        Typecheck fin backend -> case backend of
          Michelson b -> g b
          Plonk b -> g b
          where
            g :: forall b. (Show (Pipeline.Ty b), Show (Pipeline.Val b), Pipeline.HasBackend b) => b -> Pipeline.Pipeline ()
            g b = runCmd' fin b (\b -> Pipeline.parse b >=> Pipeline.typecheck @b)
        Compile fin fout backend -> case backend of
          Michelson b -> g b
          Plonk b -> g b
          where
            g :: forall b. (Show (Pipeline.Ty b), Show (Pipeline.Val b), Pipeline.HasBackend b) => b -> Pipeline.Pipeline ()
            g b = runCmd' fin b (\b -> Pipeline.parse b >=> Pipeline.typecheck @b >=> Pipeline.compile @b fout)
        Version -> liftIO $ putDoc versionDoc
        _ -> Feedback.fail "Not implemented yet."

runCmd ::
  (Show a) =>
  FilePath ->
  Backend ->
  (forall b. Pipeline.HasBackend b => b -> Text -> Pipeline.Pipeline a) ->
  Pipeline.Pipeline ()
runCmd fin backend f = case backend of
  Michelson b -> runCmd' fin b f
  Plonk b -> runCmd' fin b f

runCmd' ::
  forall a b.
  (Show a, Pipeline.HasBackend b) =>
  FilePath ->
  b ->
  (forall b. Pipeline.HasBackend b => b -> Text -> Pipeline.Pipeline a) ->
  Pipeline.Pipeline ()
runCmd' fin b f = liftIO (readFile fin) >>= f b >>= liftIO . pPrint

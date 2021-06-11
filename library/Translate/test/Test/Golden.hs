module Test.Golden (top) where

import qualified Data.ByteString as ByteString (readFile)
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Environment as Environment
import qualified Juvix.Contextify.Passes as Contextify
import qualified Juvix.Desugar.Passes as Pass
import qualified Juvix.Frontend as Frontend
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import Juvix.Frontend.Types (TopLevel)
import Juvix.Frontend.Types.Base (Header)
import qualified Juvix.Frontend.Types.Base as Frontend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Juvix.Library.Test.Golden
import Test.Tasty
import qualified Text.Pretty.Simple as Pretty
import qualified Data.List.NonEmpty as NonEmpty
import Prelude (error)

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

-- TODO: Add translations as well?

top :: IO TestTree
top =
  testGroup
    "golden tests"
    <$> sequence
      [parseTests, desugartests]

parseContract :: FilePath -> IO (Either [Char] (Header TopLevel))
parseContract file = do
  Parser.prettyParse <$> ByteString.readFile file

parseTests :: IO TestTree
parseTests =
  testGroup "parse"
    <$> sequenceA
      [ discoverGoldenTestsParse "../../test/examples/positive",
        discoverGoldenTestsParse "../../test/examples/negative"
      ]

desugartests :: IO TestTree
desugartests =
  testGroup "desugar"
    <$> sequenceA
      [ testGroup "positive" <$> discoverGoldenTestsDesugar "../../test/examples/positive",
        testGroup "negative" <$> discoverGoldenTestsDesugar "../../test/examples/negative"
      ]

contextTests :: IO TestTree
contextTests =
  testGroup "desugar"
    <$> sequenceA
      [ testGroup "positive" <$> discoverGoldenTestsDesugar "../../test/examples/positive",
        testGroup "negative" <$> discoverGoldenTestsDesugar "../../test/examples/negative"
      ]

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsParse ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsParse = discoverGoldenTests [".ju"] ".parsed" getGolden parseContract

discoverGoldenTestsDesugar ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO [TestTree]
discoverGoldenTestsDesugar filePath =
  zipWithM callGolden [0 ..] discoverDesugar
  where
    callGolden i (function, name) =
      discoverGoldenTests
        [".ju"]
        ("." <> show i <> "-" <> name)
        getGolden
        (\fileName -> function . snd <$> sexp fileName)
        filePath

discoverGoldenTestsContext ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO [TestTree]
discoverGoldenTestsContext filePath =
  zipWithM callGolden [0 ..] discoverContext
  where
    callGolden i (function, name) =
      discoverGoldenTests
        [".ju"]
        ("." <> show i <> "-" <> name)
        getGolden
        (\fileName -> function . (NonEmpty.:| [])  =<< sexp fileName)
        filePath


----------------------------------------------------------------------
-- Pass Test lists
----------------------------------------------------------------------

discoverDesugar :: IsString b => [([Sexp.T] -> [Sexp.T], b)]
discoverDesugar =
  [ (desugarModule, "desugar-module"),
    (desugarLet, "desugar-let"),
    (desugarCond, "desugar-cond"),
    (desugarIf, "desugar-if"),
    (desugarMultipleLet, "desugar-multiple-let"),
    (desugarMultipleDefun, "desugar-multiple-defun"),
    (desugarMultipleSig, "desugar-multiple-sig"),
    (desugarRemovePunnedRecords, "desugar-remove-punned-records"),
    (desugarHandlerTransform, "desugar-handler-transform")
  ]

discoverContext ::
  IsString b =>
  [(NonEmpty (NameSymbol.T, [Sexp.T]) -> IO Environment.SexpContext, b)]
discoverContext =
  [ (contextifySexp, "contextify-sexp"),
    (resolveModuleContext, "resolve-module")
  ]

----------------------------------------------------------------------
-- Desugar
----------------------------------------------------------------------

-- here we setup the long sequence that op basically does

desugarModule,
  desugarLet,
  desugarCond,
  desugarIf,
  desugarMultipleLet,
  desugarMultipleDefun,
  desugarMultipleSig,
  desugarRemovePunnedRecords,
  desugarHandlerTransform ::
    [Sexp.T] -> [Sexp.T]
desugarModule = fmap Pass.moduleLetTransform
desugarLet = fmap Pass.moduleLetTransform . desugarModule
desugarCond = fmap Pass.condTransform . desugarLet
desugarIf = fmap Pass.ifTransform . desugarCond
desugarMultipleLet = fmap Pass.multipleTransLet . desugarIf
desugarMultipleDefun = Pass.multipleTransDefun . desugarMultipleLet
desugarMultipleSig = Pass.combineSig . desugarMultipleDefun
desugarRemovePunnedRecords = fmap Pass.removePunnedRecords . desugarMultipleSig
desugarHandlerTransform = fmap Pass.handlerTransform . desugarRemovePunnedRecords

contextifySexp ::
  NonEmpty (NameSymbol.T, [Sexp.T]) -> IO Environment.SexpContext
contextifySexp names = do
  context <- Contextify.fullyContextify names
  case context of
    Left _err -> error "bad please fix me"
    Right ctx -> pure ctx

resolveModuleContext ::
  NonEmpty (NameSymbol.T, [Sexp.T]) -> IO Environment.SexpContext
resolveModuleContext names = do
  context <- Contextify.fullyContextify names
  case context of
    Left _err -> error "bad please fix me"
    Right ctx -> do
      (newCtx, _) <- Environment.runMIO (Contextify.resolveModule ctx)
      case newCtx of
        Right ctx -> pure ctx
        Left _err -> error "not valid pass"



----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

sexp :: FilePath -> IO (NameSymbol.T, [Sexp.T])
sexp path = do
  fileRead <- Frontend.ofSingleFile path
  case fileRead of
    Right (names, top) ->
      pure $ (names, fmap SexpTrans.transTopLevel top)
    Left _ -> pure $ error "failure"


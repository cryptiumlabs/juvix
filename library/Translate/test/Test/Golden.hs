module Test.Golden (top) where

import qualified Data.ByteString as ByteString (readFile)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Environment as Environment
import qualified Juvix.Contextify.Passes as Contextify
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar.Passes as Pass
import qualified Juvix.Frontend as Frontend
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import Juvix.Frontend.Types (TopLevel)
import Juvix.Frontend.Types.Base (Header)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Juvix.Library.Test.Golden
import Test.Tasty
import Prelude (error)

-- TODO: Add translations as well?

top :: IO TestTree
top =
  testGroup
    "golden tests"
    <$> sequence
      [parseTests, desugartests, contextTests]


--------------------------------------------------------------------------------
-- Parse Test Frame
--------------------------------------------------------------------------------

posNegTests ::
  (Applicative f, IsString t) => TestName -> (t -> f [TestTree]) -> f TestTree
posNegTests testName discoverFunction =
  testGroup testName
    <$> sequenceA
      [ testGroup "positive" <$> discoverFunction "../../test/examples/positive",
        testGroup "negative" <$> discoverFunction "../../test/examples/negative"
      ]


--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

parseContract :: FilePath -> IO (Either [Char] (Header TopLevel))
parseContract file = do
  Parser.prettyParse <$> ByteString.readFile file

parseTests :: IO TestTree
parseTests =
  posNegTests "parse" (\path -> fmap (\x -> [x]) (discoverGoldenTestsParse path))

desugartests :: IO TestTree
desugartests = posNegTests "desugar" discoverGoldenTestsDesugar

contextTests :: IO TestTree
contextTests = posNegTests "context" discoverGoldenTestsContext

-- | Discover golden tests for input files with extension @.ju@ and output
-- files with extension @.parsed@.
discoverGoldenTestsParse ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsParse = discoverGoldenTests [".ju"] ".parsed" getGolden parseContract

discoverGoldenTestPasses ::
  (Eq a, Show a, Read a) => (t -> FilePath -> IO a) -> [(t, [Char])] -> FilePath -> IO [TestTree]
discoverGoldenTestPasses handleDiscoverFunction discoverPasses filePath =
  zipWithM callGolden [0 ..] discoverPasses
  where
    callGolden i (passFunction, name) =
      discoverGoldenTests
        [".ju"]
        ("." <> show i <> "-" <> name)
        getGolden
        (handleDiscoverFunction passFunction)
        filePath

discoverGoldenTestsDesugar ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO [TestTree]
discoverGoldenTestsDesugar =
  discoverGoldenTestPasses handleDiscoverFunction discoverDesugar
  where
    handleDiscoverFunction desugarPass filePath =
      desugarPass . snd <$> sexp filePath

discoverGoldenTestsContext ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO [TestTree]
discoverGoldenTestsContext =
  discoverGoldenTestPasses handleDiscoverFunction discoverContext
  where
    handleDiscoverFunction contextPass filePath = do
      desugaredPath <- fullyDesugarPath (filePath : library)
      handleContextPass desugaredPath contextPass

library :: IsString a => [a]
library =
  [ "../../stdlib/Circuit.ju",
    "../../stdlib/LLVM.ju",
    "../../stdlib/MichelsonAlias.ju",
    "../../stdlib/Michelson.ju",
    "../../stdlib/Prelude.ju"
  ]

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
    (resolveModuleContext, "resolve-module"),
    (resolveInfixContext, "resolve-infix")
  ]

----------------------------------------------------------------------
-- Desugar Passes
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

fullDesugar :: [Sexp.T] -> [Sexp.T]
fullDesugar = desugarHandlerTransform

----------------------------------------------------------------------
-- Context Passes
----------------------------------------------------------------------

contextifySexp ::
  NonEmpty (NameSymbol.T, [Sexp.T]) -> IO Environment.SexpContext
contextifySexp names = do
  context <- Contextify.fullyContextify names
  case context of
    Left _err -> error "Not all modules included, please include more modules"
    Right ctx -> pure ctx

resolveModuleContext ::
  NonEmpty (NameSymbol.T, [Sexp.T]) -> IO Environment.SexpContext
resolveModuleContext names = do
  ctx <- contextifySexp names
  (newCtx, _) <- Environment.runMIO (Contextify.resolveModule ctx)
  case newCtx of
    Right ctx -> pure ctx
    Left _err -> error "not valid pass"

resolveInfixContext ::
  NonEmpty (NameSymbol.T, [Sexp.T]) -> IO Environment.SexpContext
resolveInfixContext names = do
  ctx <- resolveModuleContext names
  let (infix', _) = Environment.runM (Contextify.inifixSoloPass ctx)
  case infix' of
    Left _err -> error "can't resolve infix symbols"
    Right ctx -> pure ctx

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

handleContextPass ::
  (Monad m, Show ty, Show term, Show sumRep) =>
  [(NonEmpty Symbol, b)] ->
  (NonEmpty (NonEmpty Symbol, b) -> m (Context.T term ty sumRep)) ->
  m (Context.Record term ty sumRep)
handleContextPass desuagredSexp contextPass =
  case desuagredSexp of
    [] ->
      error "error: there are no files given"
    (moduleName, moduleDefns) : xs -> do
      let nonEmptyDesugar = (moduleName, moduleDefns) NonEmpty.:| xs
      context <- contextPass nonEmptyDesugar
      pure $ getModuleName moduleName context
  where
    getModuleName name context =
      case fmap Context.extractValue $ Context.lookup name context of
        Just (Context.Record r) ->
          r
        maybeDef ->
          error ("Definition is not a Record:" <> show maybeDef)

sexp :: FilePath -> IO (NameSymbol.T, [Sexp.T])
sexp path = do
  fileRead <- Frontend.ofSingleFile path
  case fileRead of
    Right (names, top) ->
      pure $ (names, fmap SexpTrans.transTopLevel top)
    Left _ -> pure $ error "failure"

fullyDesugarPath :: [FilePath] -> IO [(NameSymbol.T, [Sexp.T])]
fullyDesugarPath paths = do
  fileRead <- Frontend.ofPath paths
  case fileRead of
    Right xs ->
      pure $ fmap (\(names, top) -> (names, fullDesugar (fmap SexpTrans.transTopLevel top))) xs
    Left _ -> pure $ error "failure"

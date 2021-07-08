{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | @Juvix.Library.Test.Golden@ defines testing functionality for golden
--   style tests
-- - Golden tests revolve around testing files we have saved on
--   disk. Namely we wish to take that file and do some transformation
--   and save the result to compare it for regression testing.
--
-- - There are many useful sub components of this module
--
-- - The =Compact= tag to a few of the functions represents golden
--   test functions that display the results in different ways. Often
--   we use the =Compact= variant for S-expression showing as it's
--   much clearer to see what the expressions mean.
--
-- * NoQuotes
-- This structure allows us to have golden tests that are based around
-- show instances instead of normal read instances.
module Juvix.Library.Test.Golden
  ( NoQuotes (..),

    -- * Testing functionalities with the normal show and with no colors
    toNoQuotes,
    compareGolden,
    mkGoldenTest,
    discoverGoldenTests,
    discoverGoldenTestsNoQuotes,

    -- * Testing functions compact Variants
    toNoQuotesCompact,
    compareGoldenCompact,
    mkGoldenTestCompact,
    discoverGoldenTestsCompact,

    -- *
    getGolden,
    expectSuccess,
    expectFailure,
  )
where

import qualified Data.ByteString as ByteString (writeFile)
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TLazy
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath as FP
import Test.Tasty
import qualified Test.Tasty.Silver as T
import qualified Test.Tasty.Silver.Advanced as T
import Text.Pretty.Simple (pShowNoColor)
import qualified Text.Pretty.Simple as Pretty
import Text.Read (Read (..))
import qualified Prelude (show)

type FileExtension = String

newtype NoQuotes = NoQuotes Text

instance Show NoQuotes where
  show (NoQuotes t) = toS t

instance Read NoQuotes where
  readsPrec _ s = [(NoQuotes $ toS s, "")]

instance Eq NoQuotes where
  (NoQuotes s1) == (NoQuotes s2) = t1 == t2
    where
      -- TODO: This filter is potentially dangerous
      t1 = Text.filter (/= '"') . Text.strip <$> lines s1
      t2 = Text.filter (/= '"') . Text.strip <$> lines s2

toNoQuotes,
  toNoQuotesCompact ::
    (Monad m, Show a) =>
    (FilePath -> m a) ->
    FilePath ->
    m NoQuotes
toNoQuotes f filepath = do
  t <- f filepath
  pure $ NoQuotes $ toS $ pShowNoColor t
toNoQuotesCompact f filepath = do
  t <- f filepath
  pure $ NoQuotes $ toS $ printCompactParens t

getGolden :: (Read a, Show a) => FilePath -> IO (Maybe a)
getGolden file = do
  createDirectoryIfMissing True $ FP.takeDirectory file
  maybeBS <- T.readFileMaybe file

  return $ do
    bs <- maybeBS
    readMaybe $ Text.unpack $ decodeUtf8 bs

compareGoldenPretty ::
  (Eq a, Show a) => (a -> TLazy.Text) -> a -> a -> T.GDiff
compareGoldenPretty prettyPrinter golden upcoming
  | upcoming == golden =
    T.Equal
  | otherwise =
    T.DiffText
      { T.gReason =
          Just $
            "Output doesn't match golden file."
              <> "The new result is \n"
              <> toS (prettyPrinter upcoming)
              <> "\n but the expected result is \n"
              <> toS (prettyPrinter golden),
        T.gActual = resultToText upcoming,
        T.gExpected = resultToText golden
      }
  where
    resultToText :: Show a => a -> Text
    resultToText = Text.pack . show

compareGolden :: (Eq a, Show a) => a -> a -> T.GDiff
compareGolden = compareGoldenPretty pShowNoColor

compareGoldenCompact :: (Eq a, Show a) => a -> a -> T.GDiff
compareGoldenCompact = compareGoldenPretty printCompactParens

-- | Discover golden tests.
discoverGoldenTests,
  discoverGoldenTestsCompact ::
    (Show a, Eq a) =>
    -- | the input file extensions
    [FileExtension] ->
    -- | the output file extension
    FileExtension ->
    -- | get golden
    (FilePath -> IO (Maybe a)) ->
    -- | action
    (FilePath -> IO a) ->
    -- | the directory in which to recursively look for golden tests
    FilePath ->
    IO TestTree
discoverGoldenTests exts_in ext_out getGolden action path =
  testGroup path
    . map (mkGoldenTest getGolden action ext_out)
    <$> T.findByExtension exts_in path
discoverGoldenTestsCompact exts_in ext_out getGolden action path =
  testGroup path
    . map (mkGoldenTestCompact getGolden action ext_out)
    <$> T.findByExtension exts_in path

discoverGoldenTestsNoQuotes ::
  (FilePath -> FilePath) ->
  [Char] ->
  (FilePath -> IO NoQuotes) ->
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverGoldenTestsNoQuotes withJuvixRootPath ext f p = discoverGoldenTests [".ju"] ext getGolden f (withJuvixRootPath p)

toGolden :: (ConvertText a Text, ConvertText Text c) => a -> c
toGolden = toS . Text.replace "examples" "examples-golden" . toS

-- | Make a single golden test.
mkGoldenTest ::
  forall a.
  (Show a, Eq a) =>
  -- | get golden
  (FilePath -> IO (Maybe a)) ->
  -- | action
  (FilePath -> IO a) ->
  -- | the extension of the outfile, e.g. @".parsed"@
  FileExtension ->
  -- | the file path of the input file
  FilePath ->
  TestTree
mkGoldenTest getGolden action ext pathToFile =
  T.goldenTest1
    outFilename
    (getGolden outfile)
    (action pathToFile)
    compareGolden
    -- show the golden/actual value
    (T.ShowText . TLazy.toStrict . pShowNoColor)
    createOutput
  where
    directory = FP.dropFileName pathToFile
    goldenBase = FP.takeBaseName pathToFile
    outFilename = FP.replaceExtension (FP.takeFileName pathToFile) ext
    outfile = toGolden directory FP.</> goldenBase FP.</> outFilename
    createOutput =
      ByteString.writeFile outfile
        . (encodeUtf8 . TLazy.toStrict . pShowNoColor)

-- | Make a single golden test with compact Parenthesis.
mkGoldenTestCompact ::
  forall a.
  (Show a, Eq a) =>
  -- | get golden
  (FilePath -> IO (Maybe a)) ->
  -- | action
  (FilePath -> IO a) ->
  -- | the extension of the outfile, e.g. @".parsed"@
  FileExtension ->
  -- | the file path of the input file
  FilePath ->
  TestTree
mkGoldenTestCompact getGolden action ext pathToFile =
  T.goldenTest1
    outFilename
    (getGolden outfile)
    (action pathToFile)
    compareGoldenCompact
    -- show the golden/actual value
    (T.ShowText . TLazy.toStrict . printCompactParens)
    createOutput
  where
    directory = FP.dropFileName pathToFile
    goldenBase = FP.takeBaseName pathToFile
    outFilename = FP.replaceExtension (FP.takeFileName pathToFile) ext
    outfile = toGolden directory FP.</> goldenBase FP.</> outFilename
    createOutput =
      ByteString.writeFile outfile
        . (encodeUtf8 . TLazy.toStrict . printCompactParens)

expectSuccess :: (Monad m, Show (app msg)) => Feedback.FeedbackT app msg m b -> m b
expectSuccess v = do
  feedback <- Feedback.runFeedbackT v
  case feedback of
    Feedback.Success _msgs r -> pure r
    Feedback.Fail msgs -> panic $ "Expected success but failed: " <> show msgs

expectFailure :: (Monad m, Show a, Show (app msg)) => Feedback.FeedbackT app msg m a -> m NoQuotes
expectFailure v = do
  feedback <- Feedback.runFeedbackT v
  case feedback of
    Feedback.Success _msgs r -> panic $ "Expected failure but succeeded with: " <> show r
    Feedback.Fail msgs -> pure $ NoQuotes $ show msgs

printCompactParens :: Show a => a -> TLazy.Text
printCompactParens =
  Pretty.pShowOpt
    ( Pretty.defaultOutputOptionsNoColor
        { Pretty.outputOptionsCompactParens = True,
          Pretty.outputOptionsCompact = True
        }
    )

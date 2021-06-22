{-# LANGUAGE FlexibleContexts #-}

module Juvix.Library.Test.Golden
  ( getGolden,
    compareGolden,
    mkGoldenTest,
    discoverGoldenTests,
    expectSuccess,
    expectFailure,
  )
where

import qualified Data.ByteString as ByteString (writeFile)
import Data.String (String)
import qualified Data.Text as Text
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath as FP
import Test.Tasty
import qualified Test.Tasty.Silver as T
import qualified Test.Tasty.Silver.Advanced as T
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------
-- Contracts as a file (Golden tests)
--------------------------------------------------------------------------------
type FileExtension = String

getGolden :: (Read a, Show a) => FilePath -> IO (Maybe a)
getGolden file = do
  createDirectoryIfMissing True $ FP.takeDirectory file
  maybeBS <- T.readFileMaybe file
  return $ do
    bs <- maybeBS
    readMaybe $ Text.unpack $ decodeUtf8 bs

compareGolden :: (Eq a, Show a) => a -> a -> T.GDiff
compareGolden golden upcoming
  | upcoming == golden =
    T.Equal
  | otherwise =
    T.DiffText
      { T.gReason =
          Just $
            "Output doesn't match golden file."
              <> "The new result is \n"
              <> show upcoming
              <> "\n but the expected result is \n"
              <> show golden,
        T.gActual = resultToText upcoming,
        T.gExpected = resultToText golden
      }
  where
    resultToText :: Show a => a -> Text
    resultToText = Text.pack . show

-- | Discover golden tests.
discoverGoldenTests ::
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

toGolden :: (ConvertText a Text, ConvertText Text c) => a -> c
toGolden = toS . Text.replace "examples" "examples-golden" . toS

-- | Make a single golden test.
mkGoldenTest ::
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
    (T.ShowText . toS . pShowNoColor)
    createOutput
  where
    directory = FP.dropFileName pathToFile
    goldenBase = FP.takeBaseName pathToFile
    outFilename = FP.replaceExtension (FP.takeFileName pathToFile) ext
    outfile = toGolden directory FP.</> goldenBase FP.</> outFilename
    createOutput =
      ByteString.writeFile outfile
        . (encodeUtf8 . toS . pShowNoColor)

expectSuccess :: (Monad m, Show (app msg)) => Feedback.FeedbackT app msg m b -> m b
expectSuccess v = do
  feedback <- Feedback.runFeedbackT v
  case feedback of
    Feedback.Success _msgs r -> pure r
    Feedback.Fail msgs -> panic $ "Expected success but failed: " <> show msgs

expectFailure :: (Monad m, Show a) => Feedback.FeedbackT app msg m a -> m (app msg)
expectFailure v = do
  feedback <- Feedback.runFeedbackT v
  case feedback of
    Feedback.Success _msgs r -> panic $ "Expected failure but succeeded with: " <> show r
    Feedback.Fail msgs -> pure msgs

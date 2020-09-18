module Frontend.Golden where

import Data.Attoparsec.ByteString (IResult (Done, Fail, Partial))
import Data.ByteString (writeFile)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as Text
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (TopLevel)
import Juvix.Library
  ( ($),
    (.),
    ByteString,
    Eq ((==)),
    FilePath,
    IO,
    Maybe (Just),
    Monad (return),
    Semigroup ((<>)),
    Show,
    Text,
    const,
    decodeUtf8,
    encodeUtf8,
    readFile,
    readMaybe,
    writeFile,
  )
import qualified Test.Tasty as T
import qualified Test.Tasty.Silver.Advanced as T
import Prelude (show)

--------------------------------------------------------------------------------
-- Contracts as a file (Golden tests)
--------------------------------------------------------------------------------
contractFiles :: T.TestTree
contractFiles =
  T.testGroup
    "Contract Files"
    [ T.testGroup
        "Contract Files Tests - Golden"
        [ idString,
          addition,
          token
        ]
    ]

resultToText :: Show a => a -> Text
resultToText = Text.pack . show

toByteString :: Show a => a -> ByteString
toByteString = Data.ByteString.Char8.pack . show

parsedContract :: FilePath -> IO [TopLevel]
parsedContract file = do
  let failOutput i context error =
        "Failed to parse!"
          <> "The following input has not been consumed: "
          <> i
          <> "The list of context in which the error occurs is "
          <> toByteString context
          <> "The error message is "
          <> toByteString error
  let failIO i context error = do
        _ <-
          Juvix.Library.writeFile
            (file <> ".parsed")
            (decodeUtf8 $ failOutput i context error)
        return []
  let doneIO result = do
        Juvix.Library.writeFile (file <> ".parsed") (resultToText result)
        return result
  readString <- readFile file
  let rawContract = encodeUtf8 readString
  case Parser.parse rawContract of
    Fail i context err -> failIO i context err
    Done _i r -> doneIO r
    Partial cont ->
      case cont "" of
        Done _i r -> doneIO r
        Fail i context err -> failIO i context err
        Partial _cont' -> return []

getGolden :: FilePath -> IO (Maybe [TopLevel])
getGolden file = do
  maybeBS <- T.readFileMaybe file
  return
    ( do
        bs <- maybeBS
        readMaybe $ Text.unpack $ decodeUtf8 bs
    )

compareParsedGolden :: (Eq a, Show a) => a -> a -> T.GDiff
compareParsedGolden golden parsed =
  if parsed == golden
    then T.Equal
    else
      T.DiffText
        { T.gReason =
            Just $
              "Parsed output doesn't match golden file."
                <> "The parsed result is \n"
                <> show parsed
                <> "\n but the expected result is \n"
                <> show golden,
          T.gActual = resultToText parsed,
          T.gExpected = resultToText golden
        }

goldenTest :: T.TestName -> FilePath -> T.TestTree
goldenTest name file =
  let goldenFileName = file <> ".golden"
   in T.goldenTest1
        name
        (getGolden goldenFileName)
        (parsedContract file)
        compareParsedGolden
        -- show the golden/actual value, not working atm
        ( T.ShowText . Text.pack
            . (const "this isn't doing anything?") --(Prelude.unlines . map show))
              -- update the golden file, not working atm
        )
        ( Data.ByteString.writeFile goldenFileName
            . (const "this isn't either") --((encodeUtf8 . Text.pack) . ppShowList))
        )

idString :: T.TestTree
idString = goldenTest "Id-String" "test/examples/Id-Strings.ju"

addition :: T.TestTree
addition = goldenTest "Addition" "test/examples/Addition.ju"

token :: T.TestTree
token = goldenTest "Token" "test/examples/Token.ju"

-- |
-- - Generates a dot file in order to produce a simple image of a
--   interaction net
-- - Also provides a way of generating gifs (and the images used to
--   generate said gif), displaying every reduction step until the term
--   is normalized
module Juvix.Visualize.Dot where

import Control.Exception hiding ()
import qualified Data.Text as T
import Juvix.Interpreter.InteractionNet.Backends.Env
import Juvix.Interpreter.InteractionNet.Backends.Graph
import Juvix.Interpreter.InteractionNet.Nets.Default
import Juvix.Library hiding
  ( catch,
    reduce,
    throwIO,
    writeFile,
  )
import Juvix.Visualize.Graph
import System.Directory
import System.IO.Error
import Turtle hiding (FilePath, reduce)

type RunningNet primVal =
  FilePath →
  Int →
  FlipNet (Lang primVal) →
  IO (InfoNet (FlipNet (Lang primVal)))

printTestn ∷ Show b ⇒ FilePath → Either a2 (InfoNet (FlipNet b)) → IO ()
printTestn _ (Left _) = pure ()
printTestn txt (Right (InfoNet {net = net})) = showNet txt (runFlip net)

netToGif ∷ Show primVal ⇒ FilePath → RunningNet primVal
netToGif dir name num net = do
  createDirectoryIfMissing True dir
  result ← runGraphNet (dir <> "/" <> name) num net
  dirs ← listDirectory dir
  let imagesGen = T.pack <$> filter (\x → isPrefixOf name x ∧ not (T.isInfixOf "." (T.pack x))) dirs

      appDir = ((T.pack dir <> "") <>)

      packName = T.pack name

  traverse_
    ( \f → do
        removeIfExists (T.unpack (appDir (f <> ".png")))
        _ ← procStrict "dot" ["-Tpng", appDir f, "-o", appDir f <> ".png"] mempty
        removeFile (T.unpack (appDir f))
    )
    imagesGen
  removeIfExists (T.unpack (appDir (packName <> ".gif")))
  _ ←
    procStrict
      "ffmpeg"
      [ "-f",
        "image2",
        "-framerate",
        "2",
        "-i",
        appDir packName <> "%d" <> ".png",
        appDir packName <> ".gif"
      ]
      mempty
  return result

runGraphNet ∷ Show primVal ⇒ RunningNet primVal
runGraphNet name num = runFlipNetIO (reducePrint name num)

reducePrint ∷
  ( MonadIO f,
    Show primVal,
    InfoNetworkDiff FlipNet (Lang primVal) f
  ) ⇒
  FilePath →
  Int →
  f ()
reducePrint name num = flip untilNothingNTimesM num $ do
  info ← get @"info"
  ctxt ← get @"net"
  liftIO (showNet (name <> show (parallelSteps info)) (runFlip ctxt))
  reduce

removeIfExists ∷ FilePath → IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

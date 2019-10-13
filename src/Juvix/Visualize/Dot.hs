module Juvix.Visualize.Dot where

import Control.Exception hiding ()
import qualified Data.Text as T
import Juvix.Backends.Env
import Juvix.Backends.Graph
import Juvix.Library hiding
  ( catch,
    reduce,
    throwIO,
    writeFile,
  )
import Juvix.Nets.Bohm
import Juvix.Utility.Helper as H
import Juvix.Visualize.Graph
import System.Directory
import System.IO.Error
import Turtle hiding (FilePath, reduce)

printTestn :: Show b ⇒ FilePath → Either a2 (InfoNet (FlipNet b)) → IO ()
printTestn _ (Left _) = pure ()
printTestn txt (Right (InfoNet {net = net})) = showNet txt (runFlip net)

netToGif :: FilePath → FilePath → Int → FlipNet Lang → IO (InfoNet (FlipNet Lang))
netToGif dir name num net = do
  createDirectoryIfMissing True dir
  result ← runGraphNet (dir <> "/" <> name) num net
  dirs ← listDirectory dir
  let imagesGen = T.pack <$> filter (\x -> isPrefixOf name x ∧ not (T.isInfixOf "." (T.pack x))) dirs
      appDir = ((T.pack dir <> "") <>)
      packName = T.pack name
  traverse_
    ( \f -> do
        removeIfExists (T.unpack (appDir (f <> ".png")))
        _ ← procStrict "dot" ["-Tpng", appDir f, "-o", appDir f <> ".png"] mempty
        removeFile (T.unpack (appDir f))
    )
    imagesGen
  removeIfExists (T.unpack (appDir (packName <> ".gif")))
  _ ← procStrict "ffmpeg" ["-f", "image2", "-framerate", "2", "-i", appDir packName <> "%d" <> ".png", appDir packName <> ".gif"] mempty
  return result

runGraphNet :: FilePath → Int → FlipNet Lang → IO (InfoNet (FlipNet Lang))
runGraphNet name num = runFlipNetIO (reducePrint name num)

reducePrint ::
  ( MonadIO f,
    InfoNetworkDiff FlipNet Lang f
  )
    ⇒ FilePath → Int → f ()
reducePrint name num = flip H.untilNothingNTimesM num $ do
  info ← get@"info"
  ctxt ← get@"net"
  liftIO (showNet (name <> show (parallelSteps info)) (runFlip ctxt))
  reduce

removeIfExists :: FilePath → IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

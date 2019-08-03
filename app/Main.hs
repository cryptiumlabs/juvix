{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Development.GitRev
import           Options.Applicative
import           Protolude
import           System.Directory
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import           Text.RawString.QQ

import           Config
import           Interactive
import           Options

import qualified Juvix.Eal.Solve              as Solve

context ∷ IO Context
context = do
  pwd   ← getCurrentDirectory
  home  ← getHomeDirectory
  return (Context pwd home)

main ∷ IO ()
main = do
  ctx ← context
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run ctx =<< execParser opts

disclaimerDoc ∷ Doc
disclaimerDoc = mconcat [
  "This is ", red "experimental", " software – use at your own risk.",
  line,
  "Juvix may diverge from canonical protocol implementations in unexpected ways."
  ]

aboutDoc ∷ Doc
aboutDoc = mconcat [
  text "Juvix compiler & toolkit",
  line,
  text "(c) Cryptium Labs 2018-2019 • https://juvix.org",
  line,
  disclaimerDoc
  ]

versionDoc ∷ Doc
versionDoc = mconcat [
  aboutDoc,
  line <> line,
  mconcat ["Prerelease version.", line],
  mconcat ["Built from branch ", white $(gitBranch), " at commit ", magenta $(gitHash), " (commit date ", cyan $(gitCommitDate), ").", line]
  ]

interactiveDoc ∷ Doc
interactiveDoc = mconcat [
  aboutDoc,
  line,
  white [r|
     | \ \   / /\ \/ (_)
  _  | |\ \ / /  \  /| |
 | |_| | \ V /   /  \| |
  \___/   \_/   /_/\_\_|
|],
  mconcat [line, "Juvix interactive alpha. Currently supported backends: in-process.", line, "Enter :? for help. Enter :tutorial for an interactive tutorial.", line]
  ]

run ∷ Context → Options → IO ()
run ctx (Options cmd configPath) = do
  maybeConfig ← loadConfig configPath
  let conf = fromMaybe defaultConfig maybeConfig
  case cmd of
    Interactive → do
      putDoc interactiveDoc
      putStrLn ("Loaded runtime configuration from " <> configPath <> "\n")
      interactive ctx conf
      exitSuccess
    Version → do
      putDoc versionDoc
      exitSuccess
    Solve -> do
      print =<< Solve.computeTwo
    _ -> do
      putText "Not yet implemented!"
      exitFailure

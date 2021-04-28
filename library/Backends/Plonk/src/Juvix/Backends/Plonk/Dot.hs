-- | Visualise circuits using Graphviz
module Juvix.Backends.Plonk.Dot
  ( arithCircuitToDot,
    dotWriteSVG,
  )
where

import qualified Data.Text as Text
import Juvix.Backends.Plonk.Circuit (ArithCircuit (..), Gate (..), Wire (..), fetchVars)
import Juvix.Library
import System.FilePath (replaceExtension)
import System.Process.Text (readProcessWithExitCode)
import Text.PrettyPrint.Leijen.Text (Pretty (..))

arithCircuitToDot ::
  (Show f) => ArithCircuit f -> Text
arithCircuitToDot (ArithCircuit gates) =
  Text.unlines . wrapInDigraph . concatMap graphGate $ gates
  where
    wrapInDigraph x = ["digraph g {"] ++ x ++ ["}"]
    dotWire :: Wire -> Text
    dotWire = show . pretty
    dotArrow :: Text -> Text -> Text
    dotArrow s t = s <> " -> " <> t
    dotArrowLabel :: Text -> Text -> Text -> Text
    dotArrowLabel s t lbl = dotArrow s t <> " [label=\"" <> lbl <> "\"]"
    labelNode lblId lbl = lblId <> " [label=\"" <> lbl <> "\"]"
    graphGate :: Show f => Gate Wire f -> [Text]
    graphGate (MulGate lhs rhs output) =
      [ labelNode gateLabel "*",
        labelNode lhsLabel (show $ pretty lhs),
        dotArrow lhsLabel gateLabel,
        labelNode rhsLabel (show $ pretty rhs),
        dotArrow rhsLabel gateLabel
      ]
        ++ inputs lhs lhsLabel
        ++ inputs rhs rhsLabel
      where
        lhsLabel = dotWire output <> "lhs"
        rhsLabel = dotWire output <> "rhs"
        gateLabel = dotWire output
        inputs circuit tgt =
          map
            ( (\src -> dotArrowLabel src tgt (show $ pretty src))
                . dotWire
            )
            $ fetchVars circuit
    graphGate (EqualGate i m output) =
      [ labelNode gateLabel "= 0 ? 0 : 1",
        dotArrowLabel (dotWire i) gateLabel (dotWire i),
        dotArrowLabel (dotWire m) gateLabel (dotWire m)
      ]
      where
        gateLabel = dotWire output

callDot :: Text -> IO Text
callDot g = do
  (_, out, err) <- readProcessWithExitCode "dot" ["-Tsvg"] g
  if err == "" then pure out else panic err

dotWriteSVG :: FilePath -> Text -> IO ()
dotWriteSVG path = callDot >=> writeFile (replaceExtension path ".svg")

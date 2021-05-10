module FrontendDesugar where

import qualified Criterion.Main as Criterion
import Data.Attoparsec.ByteString
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.FrontendDesugar as Desugar
import Juvix.Library hiding (mod)

bench :: Criterion.Benchmark
bench =
  Criterion.bgroup
    "desugar"
    [guardTest]

guardTest :: Criterion.Benchmark
guardTest =
  let Right t = parseOnly (many Parser.topLevelSN) "let foo | x == 3 = 3 | else = 2"
   in Criterion.bgroup
        "guardTest"
        [ Criterion.bench "guard small WHNF" $
            Criterion.whnf Desugar.desugar t,
          Criterion.bench "guard small NF" $
            Criterion.nf Desugar.desugar t
        ]

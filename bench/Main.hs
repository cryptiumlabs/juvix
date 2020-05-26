module Main where

import qualified Criterion.Main as Main
import Juvix.Library

main :: IO ()
main =
  Main.defaultMain
    [ Main.bgroup
        "test"
        [ Main.bench "length" $ Main.whnf length [1 .. 10]
        ]
    ]

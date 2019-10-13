module Main where

import           Protolude

import           InteractionNet

main ∷ IO ()
main = do
  putText "Commute 1"
  print $ fullReduce trivialCommute1
  putText "Commute 2"
  print $ fullReduce trivialCommute2
  putText "Commute 3"
  print $ fullReduce trivialCommute3
  putText "Annihilate 1"
  print $ fullReduce trivialAnnihilate1
  putText "Annihilate 2"
  print $ fullReduce trivialAnnihilate2
  putText "Annihilate 3"
  print $ fullReduce trivialAnnihilate3
  putText "Nonterminating"
  print $ fullReduce nonterminating

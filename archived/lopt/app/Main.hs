module Main where

import InteractionNet
import Protolude

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

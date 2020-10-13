{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, Int, map, print, (+))

main :: IO ()
main = do
  print (f l 3 2)

l :: [Int]
l = [1, 2, 3]

f :: [Int] -> Int -> Int -> [Int]
f l a b = map (\x -> x + (a + b)) l

module Main where

import Day1.Chronal (totalFreq)

main :: IO ()
main = do
  putStrLn "Day 1: Chronal Calendar"
  print =<< totalFreq

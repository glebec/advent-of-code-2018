module Main where

import Day1.Chronal (totalOffset, firstDup)

main :: IO ()
main = do
    putStrLn "Day 1: Chronal Calendar"
    print totalOffset
    print firstDup

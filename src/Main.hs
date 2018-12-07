module Main where

import Day1.Chronal (totalOffset, firstDup)
import Day2.Inventory (checkSum, commonPart)
import Day3.Overlaps (areaOverlapped)

main :: IO ()
main = do
    putStrLn "Day 1: Chronal Calendar"
    print totalOffset
    print firstDup
    putStrLn "Day 2: Inventory Management System"
    print checkSum
    print commonPart
    putStrLn "Day 3: No Matter How You Slice It"
    print areaOverlapped

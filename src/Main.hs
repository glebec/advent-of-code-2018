module Main where

import Day1.Chronal (totalOffset, firstDup)
import Day2.Inventory (checkSum)

main :: IO ()
main = do
    putStrLn "Day 1: Chronal Calendar"
    print totalOffset
    print firstDup
    putStrLn "Day 2: Inventory Management System"
    print checkSum

module Day1.Chronal where

import Day1.Input (raw)
import qualified Data.Set as Set

readFreq :: String -> Int
readFreq s = case s of
    ('+':i) -> read i
    ('-':_) -> read s
    _ -> 0

freqs :: [Int]
freqs = readFreq <$> lines raw

-- part 1 solution
totalFreq :: Int
totalFreq = sum freqs

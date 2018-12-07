module Day1.Chronal where

import qualified Data.Set as Set
import Day1.Input (raw)

readOffset :: String -> Int
readOffset s = case s of
    ('+':i) -> read i
    ('-':_) -> read s
    _ -> 0

offsets :: [Int]
offsets = readOffset <$> lines raw

-- part 1 solution
totalOffset :: Int
totalOffset = sum offsets

freqs :: [Int]
freqs = scanl (+) 0 (cycle offsets)

-- this will find a duplicate or die trying (infinite search)
findDupInf :: Ord a => [a] -> a
findDupInf ys = go ys Set.empty where
    go (x:xs) s = if Set.member x s then x
                  else go xs (Set.insert x s)

-- part 2 solution
firstDup :: Int
firstDup = findDupInf freqs

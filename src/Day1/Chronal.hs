module Day1.Chronal where

import Data.Maybe (fromMaybe)
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

offsetCycle :: [Int]
offsetCycle = cycle offsets

freqs :: [Int]
freqs = scanl (+) 0 offsetCycle

findDup :: Ord a => [a] -> Maybe a
findDup xs = go xs Set.empty where
    go [] _ = Nothing
    go (x:xs) s = if Set.member x s
                  then Just x
                  else go xs (Set.insert x s)

-- part 2 solution
firstDup :: Int
firstDup = fromMaybe 0 (findDup freqs)

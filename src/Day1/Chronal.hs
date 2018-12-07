module Day1.Chronal where

readFreq :: String -> Int
readFreq s = case s of
    ('+':i) -> read i
    ('-':_) -> read s
    _ -> 0

input :: IO [String]
input = lines <$> readFile "src/Day1/chronal.input"

totalFreq :: IO Int
totalFreq = sum . fmap readFreq <$> input

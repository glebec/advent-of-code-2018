module Day2.Inventory where

import qualified Data.HashMap.Strict as M
import Data.Bool (bool)
import Day2.Input (raw)

ids :: [String]
ids = lines raw

countLetters :: String -> M.HashMap Char Int
countLetters = foldr incLetter M.empty where
    incLetter c = M.insertWith (+) c 1

has2s_3s :: M.HashMap Char Int -> (Bool, Bool)
has2s_3s m = let vals = M.elems m
             in (2 `elem` vals, 3 `elem` vals)

incIf :: Bool -> Int -> Int
incIf = bool id (+1)

-- part 1 solution
checkSum :: Int
checkSum = uncurry (*) p2s_3s where
    p2s_3s = foldr inc2s_3s (0, 0) ids
    inc2s_3s idx (_2s, _3s) =
        let (has2s, has3s) = (has2s_3s . countLetters) idx
        in (incIf has2s _2s, incIf has3s _3s)

differUpTo1 :: String -> String -> Bool
differUpTo1 s1 s2 = 2 `notElem` counts where
    counts = scanl (+) 0 diffs
    diffs = bool 0 1 <$> zipWith (/=) s1 s2

theFabricIds :: (String, String)
theFabricIds = head $ do
    (i, idx) <- zip [0..] ids
    idy <- snd $ splitAt (i + 1) ids
    if differUpTo1 idx idy
    then pure (idx, idy)
    else []

-- part 2 solution
commonPart :: String
commonPart = fst <$> filter (uncurry (==)) (uncurry zip theFabricIds)

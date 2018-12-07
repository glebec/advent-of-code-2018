module Day3.Overlaps where

import Text.Trifecta
import Data.Maybe (catMaybes)
import Day3.Input (raw)

claimsRaw :: [String]
claimsRaw = lines raw

data Claim =
    Claim { cId :: String
          , cX  :: Int
          , cY  :: Int
          , cW  :: Int
          , cH  :: Int } deriving (Eq, Ord, Show)

-- "#1295 @ 822,703: 23x14"
parseClaim :: Parser Claim
parseClaim = do
    let parseInt = read <$> some digit
    pId <- some (notChar ' ') <* (spaces >> char '@' >> spaces)
    pX <- parseInt <* char ','
    pY <- parseInt <* string ": "
    pW <- parseInt <* char 'x'
    pH <- parseInt
    pure $ Claim pId pX pY pW pH

resToMaybe :: Result a -> Maybe a
resToMaybe (Success x) = Just x
resToMaybe _ = Nothing

claims :: [Claim]
claims = catMaybes $ resToMaybe . parseString parseClaim mempty <$> claimsRaw

-- given a 1 square inch cell whose upper-left corner is (a, b)
cellInClaim :: Int -> Int -> Claim -> Bool
cellInClaim a b (Claim _ x y w h) =
    (x <= a && a < x + w) &&
    (y <= b && b < y + h)

cellOverlapped :: Int -> Int -> [Claim] -> Bool
cellOverlapped a b = go 0 where
    go n _ | n > 1 = True
    go n [] = False
    go n (c:cs) =
        if cellInClaim a b c
        then go (n + 1) cs
        else go n cs

getOverlap :: [Claim] -> Int
getOverlap cs = length $ filter (== True) overlaps where
    overlaps = do
        x <- [1..998]
        y <- [1..998]
        pure $ cellOverlapped x y cs

-- solution to part 1
areaOverlapped = getOverlap claims

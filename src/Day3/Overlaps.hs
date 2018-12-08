module Day3.Overlaps where

import Text.Trifecta
import qualified Data.Vector.Unboxed as V
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

type UnboxedClaim = (Int, Int, Int, Int)

vClaims :: V.Vector UnboxedClaim
vClaims = V.fromList $ (\(Claim _ x y w h) -> (x, y, w, h)) <$> claims

-- given a 1 square inch cell whose upper-left corner is (a, b)
cellInClaim :: Int -> Int -> UnboxedClaim -> Bool
cellInClaim a b (x, y, w, h) =
    (x <= a && a < x + w) &&
    (y <= b && b < y + h)

cellOverlapped :: V.Vector UnboxedClaim -> Int -> Int -> Bool
cellOverlapped cs a b = 2 `V.elem` V.scanl go (0 :: Int) cs where
    go n c = if cellInClaim a b c then n + 1 else n

getOverlap :: V.Vector UnboxedClaim -> Int
getOverlap cs = V.length $ V.filter (== True) overlaps where
    overlaps = V.map (uncurry $ cellOverlapped cs) coords
    coords = V.fromList [(x, y) | x <- dim, y <- dim]
    dim = [1..999]

-- solution to part 1
areaOverlapped = getOverlap vClaims

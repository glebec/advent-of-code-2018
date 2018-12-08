module Day3.Overlaps where

import Text.Trifecta
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Maybe (catMaybes)
import Day3.Input (raw)

claimsRaw :: [String]
claimsRaw = lines raw

data Claim = Claim
    { cId :: String
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

type Cell = (Int, Int)
type Fabric = M.Map Cell Int

claimToCells :: Claim -> [Cell]
claimToCells (Claim _ x y w h) = do
    a <- [x..x + w - 1]
    b <- [y..h + y - 1]
    pure (a, b)

addClaimToFabric :: Claim -> Fabric -> Fabric
addClaimToFabric claims f = foldr addCell f (claimToCells claims) where
    addCell cell = M.insertWith (+) cell 1

fabric :: Fabric
fabric = foldr addClaimToFabric M.empty claims

getOverlap :: Fabric -> Int
getOverlap f = length $ filter (> 1) overlaps where
    overlaps = do
        x <- [1..999]
        y <- [1..999]
        pure $ M.findWithDefault 0 (x, y) f

-- solution to part 1
areaOverlapped = getOverlap fabric

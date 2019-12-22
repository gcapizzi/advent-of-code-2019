module Lib
    ( Asteroid(..)
    , parseAsteroids
    , findBestMonitoringLocation
    , neighbours
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Ord

data Asteroid = Asteroid Int Int deriving (Eq, Ord, Show)

parseAsteroids :: Text -> Set Asteroid
parseAsteroids = S.unions . zipWith parseAsteroidsRow [0..] . T.lines . T.strip

parseAsteroidsRow :: Int -> Text -> Set Asteroid
parseAsteroidsRow y txt = S.fromList $ mapMaybe (parseAsteroid y) $ zip [0..] $ T.unpack txt

parseAsteroid :: Int -> (Int, Char) -> Maybe Asteroid
parseAsteroid y (x, char)
    | char == '#' = Just $ Asteroid x y
    | otherwise = Nothing

findBestMonitoringLocation :: Set Asteroid -> (Asteroid, Int)
findBestMonitoringLocation asteroids = maximumBy (comparing snd) asteroidsWithReachable
  where
    asteroidsWithReachable = S.map (\a -> (a, length $ asteroids `reachableFrom` a)) asteroids

neighbours :: Set Asteroid -> Asteroid -> [Asteroid]
neighbours as a
    | S.null as = []
    | otherwise = reachables ++ neighbours (S.difference as (S.fromList reachables)) a
  where
    reachables = as `reachableFrom` a

reachableFrom :: Set Asteroid -> Asteroid -> [Asteroid]
reachableFrom as a = selectClosesTo a $ groupByAngleWith a $ S.elems $ S.delete a as

selectClosesTo :: Asteroid -> [[Asteroid]] -> [Asteroid]
selectClosesTo a = map (minimumBy (comparing (distance a)))

groupByAngleWith :: Asteroid -> [Asteroid] -> [[Asteroid]]
groupByAngleWith a = groupOn (angle a) . sortOn (angle a)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

angle :: Asteroid -> Asteroid -> Double
angle (Asteroid x1 y1) (Asteroid x2 y2) = if vAngle < 0 then vAngle + 2*pi else vAngle
  where
    vAngle = vectorAngle vUp vAs
    vUp = (0, -1)
    vAs = (delta x2 x1, delta y2 y1)

vectorAngle :: (Double, Double) -> (Double, Double) -> Double
vectorAngle v1 v2 = atan2 (det v1 v2) (dot v1 v2)

dot :: (Double, Double) -> (Double, Double) -> Double
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

det :: (Double, Double) -> (Double, Double) -> Double
det (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

len :: (Double, Double) -> Double
len (x, y) = sqrt $ (x ** 2) + (y ** 2)

distance :: Asteroid -> Asteroid -> Double
distance (Asteroid x1 y1) (Asteroid x2 y2) = sqrt $ (delta y2 y1 ** 2) + (delta x2 x1 ** 2)

delta :: Int -> Int -> Double
delta x y = fromIntegral (x - y)

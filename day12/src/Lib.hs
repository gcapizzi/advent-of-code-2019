module Lib
    ( Moon(..)
    , moon
    , simulate
    , totalEnergy
    , cycleLength
    , cycleLength'
    ) where

import Debug.Trace

data Moon = Moon { position :: [Int]
                 , velocity :: [Int]
                 } deriving (Eq, Show)

moon :: [Int] -> Moon
moon position = Moon { position = position, velocity = [0, 0, 0] }

simulate :: [Moon] -> [[Moon]]
simulate = iterate step

step :: [Moon] -> [Moon]
step = map applyVelocity . applyGravity

applyVelocity :: Moon -> Moon
applyVelocity moon@Moon { position = pos, velocity = vel } = moon { position = zipWith (+) pos vel }

applyGravity :: [Moon] -> [Moon]
applyGravity moons = map (\m -> foldl applyGravityBetween m moons) moons

applyGravityBetween :: Moon -> Moon -> Moon
applyGravityBetween moon@Moon { position = pos1, velocity = vel1 } Moon { position = pos2 } = moon { velocity = newVel }
  where
    deltas = zipWith gravityDelta pos1 pos2
    newVel = zipWith (+) vel1 deltas

gravityDelta :: Int -> Int -> Int
gravityDelta a b
  | a == b = 0
  | a < b = 1
  | a > b = -1

totalEnergy :: Moon -> Int
totalEnergy Moon { position = pos, velocity = vel } = energy pos * energy vel

energy :: [Int] -> Int
energy = sum . map abs

cycleLength :: [[Moon]] -> Int
cycleLength (initialState:states) = fst $ head $ filter (\(_, s) -> s == initialState) (zip [1..] states)

cycleLength' :: [[Moon]] -> Int
cycleLength' states = foldr (lcm . (`cycleLengthForAxis` states)) 1 [0..2]

cycleLengthForAxis :: Int -> [[Moon]] -> Int
cycleLengthForAxis axis (initialState:states) = fst $ head $ filter (\(_, s) -> sameForAxis axis s initialState) (zip [1..] states)

sameForAxis :: Int -> [Moon] -> [Moon] -> Bool
sameForAxis axis s1 s2 = and $ zipWith (equalForAxis axis) s1 s2

equalForAxis :: Int -> Moon -> Moon -> Bool
equalForAxis axis Moon { position = pos1, velocity = vel1 } Moon { position = pos2, velocity = vel2 } =
    pos1 !! axis == pos2 !! axis && vel1 !! axis == vel2 !! axis

module Lib
    ( findOxygenSystem
    , discoverOxygenSystem
    , State(..)
    , printState
    , evolveOxygenSystem
    ) where

import qualified Intcode
import Data.Maybe
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord
import Data.List

import Debug.Trace

data State = State { walls :: Set (Int, Int), position :: (Int, Int), positions :: Set (Int, Int) } deriving (Eq, Show)

newState = State { walls = Set.empty, position = (0, 0), positions = Set.empty }

findOxygenSystem :: Intcode.Program -> Maybe State
findOxygenSystem = pathToOxygenSystem newState

pathToOxygenSystem :: State -> Intcode.Program -> Maybe State
pathToOxygenSystem state@State { positions = ps, position = pos } program = let
    newPs = map (\m -> (m, applyMove pos m)) [1..4]
    allowedMoves = map fst $ filter (\(_, p) -> not (p `Set.member` ps)) newPs
    states = mapMaybe (\m -> let
        newPos = applyMove pos m
        newProgram = run program m
        output = head $ Intcode.outputs newProgram
        in case output of
            0 -> Nothing
            1 -> pathToOxygenSystem state { positions = Set.insert newPos ps, position = newPos } newProgram
            2 -> Just state { positions = Set.insert newPos ps, position = newPos }) allowedMoves
    in listToMaybe states

discoverOxygenSystem :: Intcode.Program -> State
discoverOxygenSystem = explorePathToOxygenSystem newState

explorePathToOxygenSystem :: State -> Intcode.Program -> State
explorePathToOxygenSystem state@State { walls = ws, positions = ps, position = pos } program = let
    newPs = map (\m -> (m, applyMove pos m)) [1..4]
    allowedMoves = map fst $ filter (\(_, p) -> not (p `Set.member` ps)) newPs
    states = map (\m -> let
        newPos = applyMove pos m
        newProgram = run program m
        output = head $ Intcode.outputs newProgram
        in case output of
            0 -> state { walls = Set.insert newPos ws }
            1 -> explorePathToOxygenSystem state { positions = Set.insert newPos ps, position = newPos } newProgram
            2 -> state { positions = Set.insert newPos ps, position = newPos }) allowedMoves
    in foldr mergeStates newState states

mergeStates :: State -> State -> State
mergeStates State { walls = ws1, positions = ps1, position = pos1 } State { walls = ws2, positions = ps2, position = pos2 }
    = State { walls = Set.union ws1 ws2, positions = Set.union ps1 ps2, position = pos2 }

evolveOxygenSystem :: State -> State
evolveOxygenSystem s@State { walls = ws, positions = ps } = let
    newPs = Set.foldr (\o os -> (neighbours o `Set.difference` ws) `Set.union` os) ps ps
    in s { positions = newPs }

neighbours :: (Int, Int) -> Set (Int, Int)
neighbours (x, y) = Set.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

applyMove :: (Int, Int) -> Int -> (Int, Int)
applyMove (x, y) 1 = (x, y - 1)
applyMove (x, y) 2 = (x, y + 1)
applyMove (x, y) 3 = (x - 1, y)
applyMove (x, y) 4 = (x + 1, y)

run :: Intcode.Program -> Int -> Intcode.Program
run program move = let
    newProgram = Intcode.run program { Intcode.inputs = [move], Intcode.outputs = [] }
    in case newProgram of
        Left _ -> program
        Right newPrg -> newPrg

printState :: State -> String
printState state@State { walls = ws, positions = ps, position = pos } = intercalate "\n" rows ++ "\n" ++ show pos ++ " " ++ show (length ps)
  where
    rows = map (printRow state [minX..maxX]) [minY..maxY]
    maxX = maximum $ map fst $ Set.toList ws
    minX = minimum $ map fst $ Set.toList ws
    maxY = maximum $ map snd $ Set.toList ws
    minY = minimum $ map snd $ Set.toList ws

printRow :: State -> [Int] -> Int -> String
printRow state xs y = map (\x -> printCell state x y) xs

printCell :: State -> Int -> Int -> Char
printCell State { walls = ws, position = pos } x y
    | Set.member (x, y) ws = '#'
    | (x, y) == (0, 0) = '@'
    | (x, y) == pos = '$'
    | otherwise = ' '

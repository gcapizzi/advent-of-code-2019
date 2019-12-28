module Lib
    ( Screen
    , Tile(..)
    , parseScreen
    , countTiles
    , showScreen
    , updateScreen
    , findTile
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe

import Debug.Trace

data Screen = Screen { tiles :: Map (Int, Int) Tile, score :: Int }
data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq, Show)

parseScreen :: [Int] -> Screen
parseScreen = updateScreen (Screen { tiles = Map.empty, score = 0 })

updateScreen :: Screen -> [Int] -> Screen
updateScreen Screen { tiles = ts, score = s } xs = Screen { tiles = newTs, score = newS }
  where
    newTs = foldl (\m (k, t) -> Map.insert k t m) ts (map parseTile tileChunks)
    newS = if null scoreChunks then s else last $ last scoreChunks
    (tileChunks, scoreChunks) = partition ((/= -1) . head) $ chunksOf 3 xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
    | length xs < n = []
    | otherwise = take n xs:chunksOf n (drop n xs)

parseTile :: [Int] -> ((Int, Int), Tile)
parseTile (x:y:t:_) = ((x, y), tileFromInt t)

tileFromInt :: Int -> Tile
tileFromInt 0 = Empty
tileFromInt 1 = Wall
tileFromInt 2 = Block
tileFromInt 3 = Paddle
tileFromInt 4 = Ball

countTiles :: Tile -> Screen -> Int
countTiles tile Screen { tiles = ts } = length $ filter (== tile) $ Map.elems ts

showScreen :: Screen -> String
showScreen Screen { tiles = ts, score = s} = intercalate "\n" (tileRows ++ [scoreRow])
  where
    tileRows = map (\y -> showRow y [minX..maxX] ts) [minY..maxY]
    scoreRow = show s
    minX = minimum $ map fst $ Map.keys ts
    maxX = maximum $ map fst $ Map.keys ts
    minY = minimum $ map snd $ Map.keys ts
    maxY = maximum $ map snd $ Map.keys ts

showRow :: Int -> [Int] -> Map (Int, Int) Tile -> String
showRow y xs ts = map (\x -> showPixel x y ts) xs

showPixel :: Int -> Int -> Map (Int, Int) Tile -> Char
showPixel x y ts = showTile $ getTile (x, y) ts

getTile :: (Int, Int) -> Map (Int, Int) Tile -> Tile
getTile pos ts = fromMaybe Empty $ Map.lookup pos ts

showTile :: Tile -> Char
showTile Empty = ' '
showTile Wall = '#'
showTile Block = '%'
showTile Paddle = '='
showTile Ball = '*'

findTile :: Screen -> Tile -> (Int, Int)
findTile Screen { tiles = ts } t = fst $ head $ filter ((== t) . snd) $ Map.assocs ts

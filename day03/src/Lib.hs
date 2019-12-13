module Lib
    ( parsePaths
    , calculatePath
    , findIntersections
    , Segment
    , Point(..)
    , distance
    , pathDistance
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Debug.Trace
import Data.Maybe

data Direction = GoUp | GoDown | GoLeft | GoRight deriving (Show)
data Hop = Hop Direction Int deriving (Show)
type Path = [Hop]
data Point = Point Int Int deriving (Show)
data Segment = Horizontal Int Int Int | Vertical Int Int Int deriving (Show)

findIntersections :: [Segment] -> [Segment] -> [Point]
findIntersections leftPath rightPath = catMaybes [a `intersect` b | a <- leftPath, b <- rightPath ]

intersect :: Segment -> Segment -> Maybe Point
intersect Horizontal{} Horizontal{} = Nothing
intersect Vertical{} Vertical{} = Nothing
intersect (Horizontal hxStart hxEnd hy) (Vertical vx vyStart vyEnd)
    | hy `between` (vyStart, vyEnd) && vx `between` (hxStart, hxEnd) = Just (Point vx hy)
    | otherwise = Nothing
intersect (Vertical vx vyStart vyEnd) (Horizontal hxStart hxEnd hy)
    | hy `between` (vyStart, vyEnd) && vx `between` (hxStart, hxEnd) = Just (Point vx hy)
    | otherwise = Nothing

between :: Int -> (Int, Int) -> Bool
between x (a, b) = (x >= a && x <= b) || (x >=b && x <= a)

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

pathDistance :: [Segment] -> Point -> Int
pathDistance (s:rest) p
    | s `contains` p = distance (startOf s) p
    | otherwise = segmentLength s + pathDistance rest p

segmentLength :: Segment -> Int
segmentLength (Horizontal hxStart hxEnd _) = abs (hxStart - hxEnd)
segmentLength (Vertical _ hyStart hyEnd) = abs (hyStart - hyEnd)

calculatePath :: Point -> Path -> [Segment]
calculatePath start [] = []
calculatePath start (hop:rest) = segment:calculatePath (endOf segment) rest
  where
    segment = applyHop start hop

applyHop :: Point -> Hop -> Segment
applyHop (Point x y) (Hop GoUp n) = Vertical x y (y + n)
applyHop (Point x y) (Hop GoDown n) = Vertical x y (y - n)
applyHop (Point x y) (Hop GoLeft n) = Horizontal x (x - n) y
applyHop (Point x y) (Hop GoRight n) = Horizontal x (x + n) y

endOf :: Segment -> Point
endOf (Horizontal _ xEnd y) = Point xEnd y
endOf (Vertical x _ yEnd) = Point x yEnd

startOf :: Segment -> Point
startOf (Horizontal xStart _ y) = Point xStart y
startOf (Vertical x yStart _) = Point x yStart

contains :: Segment -> Point -> Bool
contains (Horizontal hxStart hxEnd hy) (Point x y) = (x `between` (hxStart, hxEnd)) && y == hy
contains (Vertical hx hyStart hyEnd) (Point x y) = x == hx && (y `between` (hyStart, hyEnd))

parsePaths :: Text -> Either String [Path]
parsePaths = mapM parsePath . T.splitOn "\n" . T.strip

parsePath :: Text -> Either String Path
parsePath = mapM parseHop . T.splitOn ","

parseHop :: Text -> Either String Hop
parseHop s = Hop <$> parseDirection (T.head s) <*> parseInt (T.tail s)

parseDirection :: Char -> Either String Direction
parseDirection 'U' = Right GoUp
parseDirection 'D' = Right GoDown
parseDirection 'L' = Right GoLeft
parseDirection 'R' = Right GoRight
parseDirection _ = Left "Unrecognized direction"

parseInt :: Text -> Either String Int
parseInt txt = fst <$> T.decimal txt

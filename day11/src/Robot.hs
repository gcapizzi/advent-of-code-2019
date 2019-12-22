module Robot
    ( Robot
    , new
    , paintShip
    , numberOfPaintedTiles
    , showGrid
    ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Intcode
import Data.List
import Data.Ord

import Debug.Trace

data Position = Position { x :: Int, y :: Int } deriving (Eq, Ord)
data Direction = N | S | W | E
data Colour = Black | White deriving (Eq, Show)
data Turn = L | R deriving (Show)

data Robot = Robot
    { position :: Position
    , direction :: Direction
    , grid :: Map Position Colour
    , program :: Intcode.Program
    }

new :: Intcode.Program -> Robot
new program = Robot
    { position = Position { x = 0, y = 0 }
    , direction = N
    , grid = Map.fromList [(Position { x = 0, y = 0 }, White)]
    , program = program
    }

paintShip :: Robot -> Either String Robot
paintShip robot@Robot{ position = pos, direction = dir, grid = grd, program = prg } = do
    let currColour = getColour pos grd
    newPrg <- Intcode.run prg { Intcode.inputs = [colourToInt currColour] }
    if null $ Intcode.outputs newPrg
    then return robot{program = newPrg}
    else do
        let newColourCode:turnCode:_ = Intcode.outputs newPrg
        let newColour = colourFromInt newColourCode
        let newGrd = setColour pos newColour grd
        let turn = turnFromInt turnCode
        let newDir = applyTurn turn dir
        let newPos = move newDir pos
        paintShip Robot { position = newPos , direction = newDir , grid = newGrd , program = newPrg { Intcode.outputs = [] }}

setColour = Map.insert
getColour pos grd = fromMaybe Black $ Map.lookup pos grd

colourToInt :: Colour -> Int
colourToInt Black = 0
colourToInt White = 1

colourFromInt :: Int -> Colour
colourFromInt 0 = Black
colourFromInt 1 = White

turnFromInt :: Int -> Turn
turnFromInt 0 = L
turnFromInt 1 = R

applyTurn :: Turn -> Direction -> Direction
applyTurn L N = W
applyTurn L S = E
applyTurn L W = S
applyTurn L E = N
applyTurn R N = E
applyTurn R S = W
applyTurn R W = N
applyTurn R E = S

move :: Direction -> Position -> Position
move N Position { x = x, y = y } = Position x (y - 1)
move S Position { x = x, y = y } = Position x (y + 1)
move W Position { x = x, y = y } = Position (x - 1) y
move E Position { x = x, y = y } = Position (x + 1) y

numberOfPaintedTiles :: Robot -> Int
numberOfPaintedTiles Robot{grid = grd} = Map.size grd

showGrid :: Robot -> String
showGrid Robot{grid = grd} = intercalate "\n" $ map (\y -> showRow y [minX..maxX] grd) [minY..maxY]
  where
    minX = minimum $ map x $ Map.keys grd
    maxX = maximum $ map x $ Map.keys grd
    minY = minimum $ map x $ Map.keys grd
    maxY = maximum $ map y $ Map.keys grd

showRow :: Int -> [Int] -> Map Position Colour -> String
showRow y xs grd = map (\x -> showPanel x y grd) xs

showPanel :: Int -> Int -> Map Position Colour -> Char
showPanel x y grd = showColour $ getColour (Position { x = x, y = y }) grd

showColour :: Colour -> Char
showColour Black = '.'
showColour White = '#'

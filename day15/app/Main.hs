module Main where

import Lib
import System.Environment
import qualified Intcode
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  let program = Intcode.parse sourceCode
  case program of
    Left msg -> fail msg
    Right prg -> do
      let stateWithWalls = discoverOxygenSystem prg
      let stateWithOxygenSystem = findOxygenSystem prg
      case stateWithOxygenSystem of
        Nothing -> fail "boo"
        Just so -> do
          let startingState = State { walls = walls stateWithWalls
                                    , positions = Set.singleton (position so)
                                    , position = (0, 0)
                                    }
          let states = iterate evolveOxygenSystem startingState
          print $ length $ takeWhile (uncurry (/=)) $ zip states (tail states)

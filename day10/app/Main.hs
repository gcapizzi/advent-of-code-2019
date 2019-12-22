module Main where

import Lib
import System.Environment
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  txt <- TIO.readFile $ head args
  let asteroids = parseAsteroids txt
  let (bestLocation, numOfReachable) = findBestMonitoringLocation asteroids
  putStrLn $ "Best location: " ++ show bestLocation ++ " (reaches " ++ show numOfReachable ++ " asteroids)"
  let listOfNeighbours = neighbours asteroids bestLocation
  print $ listOfNeighbours !! 199

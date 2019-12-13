module Main where

import Lib
import System.Environment
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.List

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile $ head args
  print $ closestDistance input

closestDistance :: Text -> Either String Int
closestDistance input = do
  paths <- parsePaths input
  let firstPath = calculatePath (Point 0 0) (head paths)
  let secondPath = calculatePath (Point 0 0) (paths !! 1)
  let intersections = findIntersections firstPath secondPath
  let firstPathDistances = map (pathDistance firstPath) intersections
  let secondPathDistances = map (pathDistance secondPath) intersections
  let totalDistances = zipWith (+) firstPathDistances secondPathDistances
  return $ minimum totalDistances

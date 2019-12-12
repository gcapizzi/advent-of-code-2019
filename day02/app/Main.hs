module Main where

import System.Environment
import Lib
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  print $ findInputs 19690720 sourceCode

findInputs :: Int -> Text -> Either String [(Int, Int)]
findInputs output sourceCode = do
  program <- parseProgram sourceCode
  let solutions = [(x, y) | x <- [0..99], y <- [0..99], runProgramWithParams program x y == Right output]
  return solutions

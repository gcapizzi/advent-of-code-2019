module Main where

import System.Environment
import qualified Intcode
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  print $ findInputs 19690720 sourceCode

findInputs :: Int -> Text -> [(Int, Int)]
findInputs output sourceCode =
  [(noun, verb) | noun <- [0..99], verb <- [0..99], Intcode.runWithParams sourceCode noun verb == Right output]

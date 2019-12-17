module Main where

import Intcode
import System.Environment
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  let inputs = map read $ tail args
  print $ Intcode.run (sourceCode, inputs)

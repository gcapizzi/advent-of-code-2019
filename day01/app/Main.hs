module Main where

import System.Environment
import Lib
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile $ head args
  print $ calculateTotalFuel input

module Main where

import Lib
import System.Environment
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile $ head args
  print $ countOrbits input
  print $ orbitalTransfers input

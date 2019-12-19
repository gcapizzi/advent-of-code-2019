module Main where

import Lib
import System.Environment
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  print $ findMaxThrusterSignalInLoop sourceCode

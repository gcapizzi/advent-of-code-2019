module Main where

import System.Environment
import qualified Intcode
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  print $ Intcode.run (sourceCode, [5])

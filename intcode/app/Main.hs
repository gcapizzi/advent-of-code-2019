module Main where

import qualified Intcode
import System.Environment
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  let inputs = map read $ tail args
  print $ run sourceCode inputs

run :: Text -> [Int] -> Either String Intcode.Program
run sourceCode inputs = do
    program <- Intcode.parse sourceCode
    Intcode.run program { Intcode.inputs = inputs }

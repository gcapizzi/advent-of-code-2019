module Main where

import System.Environment
import qualified Intcode
import qualified Data.Text.IO as TIO
import Data.Text (Text)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  print $ Intcode.outputs <$> run sourceCode [2]

run :: Text -> [Int] -> Either String Intcode.Program
run sourceCode inputs = do
    program <- Intcode.parse sourceCode
    Intcode.run program { Intcode.inputs = inputs }

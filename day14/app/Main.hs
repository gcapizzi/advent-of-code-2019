module Main where

import Lib
import System.Environment
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  input <- TIO.readFile $ head args
  let reactionBook = parseReactions input
  case reactionBook of
    Left msg -> fail msg
    Right b -> do
      print $ costOfReaction b 1 (Element "FUEL")
      print $ 1000000000000 - costOfReaction b 3281820 (Element "FUEL")


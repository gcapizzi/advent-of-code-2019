module Main where

import Lib
import System.Environment
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  imgTxt <- TIO.readFile $ head args
  let img = parseImage 25 6 imgTxt
  print $ checksum img
  putStrLn $ T.unpack $ renderImage img

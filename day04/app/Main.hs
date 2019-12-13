module Main where

import Lib
import System.Environment
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  let input = T.pack $ head args
  print $ length <$> crackPasswords input

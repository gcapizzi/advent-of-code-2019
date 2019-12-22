module Main where

import Robot (Robot)
import qualified Robot
import qualified Intcode
import System.Environment
import qualified Data.Text.IO as TIO
import Data.Text (Text)

import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  txt <- TIO.readFile $ head args
  case runRobot txt of
    Right newRobot -> putStrLn $ Robot.showGrid newRobot
    Left err -> putStrLn err

runRobot :: Text -> Either String Robot
runRobot txt = Intcode.parse txt >>= (Robot.paintShip . Robot.new)

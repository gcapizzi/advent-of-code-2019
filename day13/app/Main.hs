module Main where

import Lib
import System.Environment
import qualified Intcode
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Control.Concurrent
import Data.Either
import qualified Data.IntMap.Strict as IntMap
import System.IO

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  let program = Intcode.parse sourceCode
  case program of
    Left msg -> fail msg
    Right prg -> do
      let freePrg = prg { Intcode.instructions = IntMap.insert 0 2 (Intcode.instructions prg) }
      let runningProgram = Intcode.run freePrg
      case runningProgram of
        Left msg -> fail msg
        Right runningPrg -> do
          let screen = parseScreen $ Intcode.outputs runningPrg
          runGame' screen runningPrg

runGame :: Screen -> Intcode.Program -> IO ()
runGame screen program = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clear
  putStrLn $ showScreen screen
  c <- getChar
  let i = case c of
            'd' -> 1
            'a' -> -1
            _   -> 0
  let newProgram = Intcode.run program { Intcode.inputs = [i] }
  case newProgram of
    Left msg -> fail msg
    Right newPrg -> do
      let newScreen = updateScreen screen $ Intcode.outputs newPrg
      runGame newScreen newPrg { Intcode.outputs = [] }

runGame' :: Screen -> Intcode.Program -> IO ()
runGame' screen program = do
  clear
  putStrLn $ showScreen screen
  threadDelay 100000
  let paddleX = fst $ findTile screen Paddle
  let ballX = fst $ findTile screen Ball
  let i | paddleX < ballX = 1
        | paddleX > ballX = -1
        | otherwise = 0
  let newProgram = Intcode.run program { Intcode.inputs = [i] }
  case newProgram of
    Left msg -> fail msg
    Right newPrg -> do
      let newScreen = updateScreen screen $ Intcode.outputs newPrg
      runGame' newScreen newPrg { Intcode.outputs = [] }

clear :: IO ()
clear = putStr "\ESC[2J"

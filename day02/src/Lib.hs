module Lib
    ( Program
    , parseProgram
    , unparseProgram
    , runProgram
    , runProgramWithParams
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import TextShow
import Data.Maybe

type Program = Vector Int

parseProgram :: Text -> Either String Program
parseProgram program = do
    let txtInstructions = T.splitOn "," program
    intInstructions <- mapM parseInt txtInstructions
    return $ V.fromList intInstructions

unparseProgram :: Program -> Text
unparseProgram = T.intercalate "," . V.toList . V.map showt

runProgram :: Program -> Either String Program
runProgram = runProgramFrom 0

runProgramWithParams :: Program -> Int -> Int -> Either String Int
runProgramWithParams program firstParam secondParam = do
    program' <- update program 1 firstParam
    program'' <- update program' 2 secondParam
    newProgram <- runProgram program''
    return $ V.head newProgram

runProgramFrom :: Int -> Program -> Either String Program
runProgramFrom address program = do
    opCode <- get program address
    case opCode of
        1 -> runOp (+) address program >>= runProgramFrom (address + 4)
        2 -> runOp (*) address program >>= runProgramFrom (address + 4)
        99 -> Right program

runOp :: (Int -> Int -> Int) -> Int -> Program -> Either String Program
runOp op address program = do
    leftAddress <- get program (address + 1)
    rightAddress <- get program (address + 2)
    resultAddress <- get program (address + 3)
    leftValue <- get program leftAddress
    rightValue <- get program rightAddress
    update program resultAddress (leftValue `op` rightValue)

parseInt :: Text -> Either String Int
parseInt txt = fst <$> T.decimal txt

get :: Program -> Int -> Either String Int
get program address = maybe (Left "Invalid address") Right (program V.!? address)

update :: Program -> Int -> Int -> Either String Program
update program address value
    | address < V.length program = Right $ program V.// [(address, value)]
    | otherwise = Left "Invalid address"

module Intcode
    ( run
    , runWithParams
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import TextShow
import Data.Maybe

type Program = Vector Int

run :: Text -> Either String Text
run sourceCode = do
  program <- parseProgram sourceCode
  newProgram <- runProgram program
  return $ unparseProgram newProgram

parseProgram :: Text -> Either String Program
parseProgram program = do
    let txtInstructions = T.splitOn "," program
    intInstructions <- mapM parseInt txtInstructions
    return $ V.fromList intInstructions

parseInt :: Text -> Either String Int
parseInt txt = fst <$> T.decimal txt

runProgram :: Program -> Either String Program
runProgram = runProgramFrom 0

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

get :: Program -> Int -> Either String Int
get program address = maybe (Left "Invalid address") Right (program V.!? address)

update :: Program -> Int -> Int -> Either String Program
update program address value
    | address < V.length program = Right $ program V.// [(address, value)]
    | otherwise = Left "Invalid address"

runWithParams :: Text -> Int -> Int -> Either String Int
runWithParams sourceCode noun verb = do
    program <- parseProgram sourceCode
    program' <- update program 1 noun
    program'' <- update program' 2 verb
    newProgram <- runProgram program''
    return $ V.head newProgram

unparseProgram :: Program -> Text
unparseProgram = T.intercalate "," . V.toList . V.map showt

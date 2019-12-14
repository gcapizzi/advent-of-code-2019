module Intcode
    ( run
    , runWithParams
    , runWithIO
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import TextShow
import Data.Maybe

type Program = Vector Int
data State = State { program :: Program, address :: Int, inputs :: [Int], outputs :: [Int] }

run :: Text -> Either String Text
run sourceCode = fst <$> runWithIO (sourceCode, [])

runWithIO :: (Text, [Int]) -> Either String (Text, [Int])
runWithIO (sourceCode, is) = do
    prg <- parseProgram sourceCode
    (newPrg, os) <- runProgram (prg, is)
    return (unparseProgram newPrg, os)

parseProgram :: Text -> Either String Program
parseProgram prg = do
    let txtInstructions = T.splitOn "," prg
    intInstructions <- mapM parseInt txtInstructions
    return $ V.fromList intInstructions

parseInt :: Text -> Either String Int
parseInt txt = fst <$> T.decimal txt

runProgram :: (Program, [Int]) -> Either String (Program, [Int])
runProgram (prg, is) = do
    newState <- runProgramWith State
        { address = 0
        , program = prg
        , inputs = is
        , outputs = []
        }
    return (program newState, outputs newState)

runProgramWith :: State -> Either String State
runProgramWith state@State { program=prg, address=addr, inputs=is, outputs=os } = do
    opCode <- get prg addr
    case opCode of
        1 -> runOp (+) state >>= runProgramWith
        2 -> runOp (*) state >>= runProgramWith
        3 -> do
            destAddress <- get prg (addr + 1)
            newPrg <- set prg destAddress (head is)
            runProgramWith $ state { program = newPrg, address = addr + 2, inputs = tail is }
        4 -> do
            sourceAddress <- get prg (addr + 1)
            value <- get prg sourceAddress
            runProgramWith $ state { address = addr + 2, outputs = value:os }
        99 -> return state

runOp :: (Int -> Int -> Int) -> State -> Either String State
runOp op state@State{program = prg, address = addr} = do
    leftAddress <- get prg (addr + 1)
    rightAddress <- get prg (addr + 2)
    resultAddress <- get prg (addr + 3)
    leftValue <- get prg leftAddress
    rightValue <- get prg rightAddress
    newPrg <- set prg resultAddress (leftValue `op` rightValue)
    return state { program = newPrg, address = addr + 4 }

get :: Program -> Int -> Either String Int
get program address = maybe (Left "Invalid address") Right (program V.!? address)

set :: Program -> Int -> Int -> Either String Program
set program address value
    | address < V.length program = Right $ program V.// [(address, value)]
    | otherwise = Left "Invalid address"

unparseProgram :: Program -> Text
unparseProgram = T.intercalate "," . V.toList . V.map showt

runWithParams :: Text -> Int -> Int -> Either String Int
runWithParams sourceCode noun verb = do
    program <- parseProgram sourceCode
    program' <- set program 1 noun
    program'' <- set program' 2 verb
    (newProgram, _) <- runProgram (program'', [])
    return $ V.head newProgram

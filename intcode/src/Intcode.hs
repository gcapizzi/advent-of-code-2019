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

data ParameterMode = Position | Immediate deriving (Eq)
data Instruction = Add | Multiply | Set | Get | Exit
data OpCode = OpCode Instruction [ParameterMode]

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
parseInt txt
    | T.head txt == '-' = negate <$> parseInt (T.tail txt)
    | otherwise = fst <$> T.decimal txt

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
    (OpCode instruction parameterModes) <- parseOpCode opCode
    case instruction of
        Add -> runBinaryOperation (+) parameterModes state >>= runProgramWith
        Multiply -> runBinaryOperation (*) parameterModes state >>= runProgramWith
        Set -> runSet state >>= runProgramWith
        Get -> runGet parameterModes state >>= runProgramWith
        Exit -> return state

runBinaryOperation :: (Int -> Int -> Int) -> [ParameterMode] -> State -> Either String State
runBinaryOperation op parameterModes state@State{program = prg, address = addr} = do
    let leftParameterMode = head parameterModes
    leftParameter <- get prg (addr + 1)
    let rightParameterMode = parameterModes !! 1
    rightParameter <- get prg (addr + 2)
    resultAddress <- get prg (addr + 3)
    leftValue <- if leftParameterMode == Immediate then Right leftParameter else get prg leftParameter
    rightValue <- if rightParameterMode == Immediate then Right rightParameter else get prg rightParameter
    newPrg <- set prg resultAddress (leftValue `op` rightValue)
    return state { program = newPrg, address = addr + 4 }

runSet :: State -> Either String State
runSet state@State { program = prg, address = addr, inputs = is } = do
    destAddress <- get prg (addr + 1)
    newPrg <- set prg destAddress (head is)
    return state { program = newPrg, address = addr + 2, inputs = tail is }

runGet :: [ParameterMode] -> State -> Either String State
runGet parameterModes state@State { program = prg, address = addr, outputs = os } = do
    let sourceParameterMode = head parameterModes
    sourceParameter <- get prg (addr + 1)
    value <- if sourceParameterMode == Immediate then Right sourceParameter else get prg sourceParameter
    return state { address = addr + 2, outputs = value:os }

get :: Program -> Int -> Either String Int
get program address = maybe (Left "Invalid address") Right (program V.!? address)

set :: Program -> Int -> Int -> Either String Program
set program address value
    | address < V.length program = Right $ program V.// [(address, value)]
    | otherwise = Left "Invalid address"

unparseProgram :: Program -> Text
unparseProgram = T.intercalate "," . V.toList . V.map showt

parseOpCode :: Int -> Either String OpCode
parseOpCode opCode = do
  instruction <- parseInstruction (opCode `mod` 100)
  parameterModes <- parseParameterModes (opCode `div` 100)
  return $ OpCode instruction parameterModes

parseInstruction :: Int -> Either String Instruction
parseInstruction 1 = Right Add
parseInstruction 2 = Right Multiply
parseInstruction 3 = Right Set
parseInstruction 4 = Right Get
parseInstruction 99 = Right Exit
parseInstruction opCode = Left $ "Unrecognized instruction: " ++ show opCode

parseParameterModes :: Int -> Either String [ParameterMode]
parseParameterModes x = do
    providedParameterModes <- mapM parseParameterMode (show x)
    return $ reverse providedParameterModes ++ repeat Position

parseParameterMode :: Char -> Either String ParameterMode
parseParameterMode '0' = Right Position
parseParameterMode '1' = Right Immediate
parseParameterMode x = Left $ "Unrecognized parameter mode: " ++ show x

runWithParams :: Text -> Int -> Int -> Either String Int
runWithParams sourceCode noun verb = do
    program <- parseProgram sourceCode
    program' <- set program 1 noun
    program'' <- set program' 2 verb
    (newProgram, _) <- runProgram (program'', [])
    return $ V.head newProgram

module Intcode
    ( Program(..)
    , parse
    , run
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import TextShow
import Data.Maybe

data Program = Program { instructions :: Vector Int, address :: Int, inputs :: [Int], outputs :: [Int] }
  deriving (Eq, Show)

data OpCode = OpCode InstructionType [ParameterMode]
data InstructionType = Add | Multiply | Set | Get | JumpIfTrue | JumpIfFalse | LessThan | Equals | Exit
data ParameterMode = Position | Immediate deriving (Eq)

parse :: Text -> Either String Program
parse sourceCode = do
    let txtInstructions = T.splitOn "," sourceCode
    intInstructions <- mapM parseInt txtInstructions
    return $ Program { instructions = V.fromList intInstructions, address = 0, inputs = [], outputs = [] }

parseInt :: Text -> Either String Int
parseInt txt
    | T.head txt == '-' = negate <$> parseInt (T.tail txt)
    | otherwise = fst <$> T.decimal txt

run :: Program -> Either String Program
run program@Program { instructions=ins, address=addr, inputs=is, outputs=os } = do
    opCode <- get ins addr
    (OpCode instruction parameterModes) <- parseOpCode opCode
    case instruction of
        Add -> runBinaryOperation (+) parameterModes program
        Multiply -> runBinaryOperation (*) parameterModes program
        Set -> runSet program
        Get -> runGet parameterModes program
        JumpIfTrue -> runJumpIf True parameterModes program
        JumpIfFalse -> runJumpIf False parameterModes program
        LessThan -> runBinaryOperation lessThan parameterModes program
        Equals -> runBinaryOperation equals parameterModes program
        Exit -> return program

runBinaryOperation :: (Int -> Int -> Int) -> [ParameterMode] -> Program -> Either String Program
runBinaryOperation op parameterModes program@Program{instructions = ins, address = addr} = do
    let leftParameterMode = head parameterModes
    leftValue <- getWithMode leftParameterMode ins (addr + 1)
    let rightParameterMode = parameterModes !! 1
    rightValue <- getWithMode rightParameterMode ins (addr + 2)
    resultAddress <- get ins (addr + 3)
    newIns <- set ins resultAddress (leftValue `op` rightValue)
    run program { instructions = newIns, address = addr + 4 }

lessThan :: Int -> Int -> Int
lessThan x y
    | x < y = 1
    | otherwise = 0

equals :: Int -> Int -> Int
equals x y
    | x == y = 1
    | otherwise = 0

runSet :: Program -> Either String Program
runSet program@Program { inputs = [] } = return program
runSet program@Program { instructions = ins, address = addr, inputs = is } = do
    destAddress <- get ins (addr + 1)
    newIns <- set ins destAddress (head is)
    run program { instructions = newIns, address = addr + 2, inputs = tail is }

runGet :: [ParameterMode] -> Program -> Either String Program
runGet parameterModes program@Program { instructions = ins, address = addr, outputs = os } = do
    let parameterMode = head parameterModes
    value <- getWithMode parameterMode ins (addr + 1)
    run program { address = addr + 2, outputs = value:os }

runJumpIf :: Bool -> [ParameterMode] -> Program -> Either String Program
runJumpIf nonZero parameterModes program@Program{instructions = prg, address = addr} = do
    let conditionParameterMode = head parameterModes
    conditionValue <- getWithMode conditionParameterMode prg (addr + 1)
    let destinationParameterMode = parameterModes !! 1
    destinationValue <- getWithMode destinationParameterMode prg (addr + 2)
    if (conditionValue /= 0) == nonZero
        then run program { address = destinationValue }
        else run program { address = addr + 3 }

get :: Vector Int -> Int -> Either String Int
get program address = maybe (Left "Invalid address") Right (program V.!? address)

getWithMode :: ParameterMode -> Vector Int -> Int -> Either String Int
getWithMode Immediate program address = get program address
getWithMode Position program address = get program address >>= get program

set :: Vector Int -> Int -> Int -> Either String (Vector Int)
set instructions address value
    | address < V.length instructions = Right $ instructions V.// [(address, value)]
    | otherwise = Left "Invalid address"

parseOpCode :: Int -> Either String OpCode
parseOpCode opCode = do
  instruction <- parseInstructionType (opCode `mod` 100)
  parameterModes <- parseParameterModes (opCode `div` 100)
  return $ OpCode instruction parameterModes

parseInstructionType :: Int -> Either String InstructionType
parseInstructionType 1 = Right Add
parseInstructionType 2 = Right Multiply
parseInstructionType 3 = Right Set
parseInstructionType 4 = Right Get
parseInstructionType 5 = Right JumpIfTrue
parseInstructionType 6 = Right JumpIfFalse
parseInstructionType 7 = Right LessThan
parseInstructionType 8 = Right Equals
parseInstructionType 99 = Right Exit
parseInstructionType opCode = Left $ "Unrecognized instruction type: " ++ show opCode

parseParameterModes :: Int -> Either String [ParameterMode]
parseParameterModes x = do
    providedParameterModes <- mapM parseParameterMode (show x)
    return $ reverse providedParameterModes ++ repeat Position

parseParameterMode :: Char -> Either String ParameterMode
parseParameterMode '0' = Right Position
parseParameterMode '1' = Right Immediate
parseParameterMode x = Left $ "Unrecognized parameter mode: " ++ show x

module Intcode
    ( Program(..)
    , parse
    , run
    ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import TextShow
import Data.Maybe

import Debug.Trace

data Program = Program
    { instructions :: IntMap Int
    , address :: Int
    , inputs :: [Int]
    , outputs :: [Int]
    , relativeBase :: Int
    }
    deriving (Eq, Show)

data OpCode = OpCode InstructionType [ParameterMode] deriving (Eq, Show)

data InstructionType =
    Add
  | Multiply
  | Set
  | Get
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | SetRelativeBase
  | Exit
  deriving (Eq, Show)

data ParameterMode =
    Position
  | Immediate
  | Relative
  deriving (Eq, Show)

parse :: Text -> Either String Program
parse sourceCode = do
    let txtInstructions = T.splitOn "," sourceCode
    intInstructions <- mapM parseInt txtInstructions
    return $ Program
        { instructions = M.fromList (zip [0..] intInstructions)
        , address = 0
        , inputs = []
        , outputs = []
        , relativeBase = 0
        }

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
        Set -> runSet parameterModes program
        Get -> runGet parameterModes program
        JumpIfTrue -> runJumpIf True parameterModes program
        JumpIfFalse -> runJumpIf False parameterModes program
        LessThan -> runBinaryOperation lessThan parameterModes program
        Equals -> runBinaryOperation equals parameterModes program
        SetRelativeBase -> runSetRelativeBase parameterModes program
        Exit -> return program

runBinaryOperation :: (Int -> Int -> Int) -> [ParameterMode] -> Program -> Either String Program
runBinaryOperation op parameterModes program@Program{instructions = ins, address = addr} = do
    let leftParameterMode = head parameterModes
    leftValue <- getWithMode leftParameterMode program (addr + 1)
    let rightParameterMode = parameterModes !! 1
    rightValue <- getWithMode rightParameterMode program (addr + 2)
    let resultParameterMode = parameterModes !! 2
    resultValue <- get ins (addr + 3)
    newProgram <- setWithMode resultParameterMode program resultValue (leftValue `op` rightValue)
    run newProgram { address = addr + 4 }

lessThan :: Int -> Int -> Int
lessThan x y
    | x < y = 1
    | otherwise = 0

equals :: Int -> Int -> Int
equals x y
    | x == y = 1
    | otherwise = 0

runSet :: [ParameterMode] -> Program -> Either String Program
runSet _ program@Program { inputs = [] } = return program
runSet parameterModes program@Program { instructions = ins, address = addr, inputs = is } = do
    let destParameterMode = head parameterModes
    destValue <- get ins (addr + 1)
    newProgram <- setWithMode destParameterMode program destValue (head is)
    run newProgram { address = addr + 2, inputs = tail is }

runGet :: [ParameterMode] -> Program -> Either String Program
runGet parameterModes program@Program { instructions = ins, address = addr, outputs = os } = do
    let parameterMode = head parameterModes
    value <- getWithMode parameterMode program (addr + 1)
    run program { address = addr + 2, outputs = value:os }

runJumpIf :: Bool -> [ParameterMode] -> Program -> Either String Program
runJumpIf nonZero parameterModes program@Program{instructions = ins, address = addr} = do
    let conditionParameterMode = head parameterModes
    conditionValue <- getWithMode conditionParameterMode program (addr + 1)
    let destinationParameterMode = parameterModes !! 1
    destinationValue <- getWithMode destinationParameterMode program (addr + 2)
    if (conditionValue /= 0) == nonZero
        then run program { address = destinationValue }
        else run program { address = addr + 3 }

runSetRelativeBase :: [ParameterMode] -> Program -> Either String Program
runSetRelativeBase parameterModes program@Program{instructions = ins, address = addr, relativeBase = rb} = do
    let parameterMode = head parameterModes
    value <- getWithMode parameterMode program (addr + 1)
    run program { address = addr + 2, relativeBase = rb + value }

getWithMode :: ParameterMode -> Program -> Int -> Either String Int
getWithMode Position Program{instructions = ins} address = do
    ref <- get ins address
    get ins ref
getWithMode Immediate Program{instructions = ins} address = get ins address
getWithMode Relative Program{instructions = ins, relativeBase = rb} address = do
    ref <- get ins address
    get ins (ref + rb)

get :: IntMap Int -> Int -> Either String Int
get instructions address
    | address >= 0 = Right $ fromMaybe 0 (instructions M.!? address)
    | otherwise = Left "Invalid address"

setWithMode :: ParameterMode -> Program -> Int -> Int -> Either String Program
setWithMode Position program@Program{instructions = ins} address value = do
    newIns <- set ins address value
    return program { instructions = newIns }
setWithMode Immediate _ _ _ = Left "Invalid parameter mode: Immediate"
setWithMode Relative program@Program{instructions = ins, relativeBase = rb} address value = do
    newIns <- set ins (address + rb) value
    return program { instructions = newIns }

set :: IntMap Int -> Int -> Int -> Either String (IntMap Int)
set instructions address value
    | address >= 0 = Right $ M.insert address value instructions
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
parseInstructionType 9 = Right SetRelativeBase
parseInstructionType 99 = Right Exit
parseInstructionType opCode = Left $ "Unrecognized instruction type: " ++ show opCode

parseParameterModes :: Int -> Either String [ParameterMode]
parseParameterModes x = do
    providedParameterModes <- mapM parseParameterMode (show x)
    return $ reverse providedParameterModes ++ repeat Position

parseParameterMode :: Char -> Either String ParameterMode
parseParameterMode '0' = Right Position
parseParameterMode '1' = Right Immediate
parseParameterMode '2' = Right Relative
parseParameterMode x = Left $ "Unrecognized parameter mode: " ++ show x

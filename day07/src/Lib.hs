module Lib
    ( findMaxThrusterSignal
    , findMaxThrusterSignalInLoop
    ) where

import Data.Text (Text)
import qualified Intcode
import Data.List
import Data.Foldable

findMaxThrusterSignal :: Text -> Either String Int
findMaxThrusterSignal sourceCode = do
    program <- Intcode.parse sourceCode
    signals <- mapM (runAmplifiersWithPhases program) (permutations [0..4])
    return $ maximum signals

runAmplifiersWithPhases :: Intcode.Program -> [Int] -> Either String Int
runAmplifiersWithPhases program = foldlM (runAmplifierWithPhase program) 0

runAmplifierWithPhase :: Intcode.Program -> Int -> Int -> Either String Int
runAmplifierWithPhase program input phase = do
    newProgram <- Intcode.run $ program { Intcode.inputs = [phase, input] }
    return $ head $ Intcode.outputs newProgram

findMaxThrusterSignalInLoop :: Text -> Either String Int
findMaxThrusterSignalInLoop sourceCode = do
    program <- Intcode.parse sourceCode
    signals <- mapM (runAmplifiersWithPhasesInLoop program) (permutations [5..9])
    return $ maximum signals

runAmplifiersWithPhasesInLoop :: Intcode.Program -> [Int] -> Either String Int
runAmplifiersWithPhasesInLoop program phases = runAmplifiersLoop 0 amps
  where
    amps = initAmplifiersWithPhases program phases

initAmplifiersWithPhases :: Intcode.Program -> [Int] -> [Intcode.Program]
initAmplifiersWithPhases program = map (\p -> program { Intcode.inputs = [p] })

runAmplifiersLoop :: Int -> [Intcode.Program] -> Either String Int
runAmplifiersLoop i amps = do
    let index = calculateIndex (length amps)
    let prevAmp = amps !! index (i - 1)
    let prevOutputs = Intcode.outputs prevAmp
    let inputs = if null prevOutputs then [0] else prevOutputs
    let amp = amps !! index i
    newAmp <- Intcode.run amp { Intcode.inputs = Intcode.inputs amp ++ inputs }
    let newAmps = replace amps i newAmp
    let newAmps' = replace newAmps (index (i - 1)) prevAmp { Intcode.outputs = [] }
    let outputs = Intcode.outputs newAmp
    if null outputs
        then return $ head prevOutputs
        else runAmplifiersLoop (index (i + 1)) newAmps'

calculateIndex :: Int -> Int -> Int
calculateIndex size i
    | i < 0 = calculateIndex size (i + size)
    | otherwise = i `mod` size

replace :: [a] -> Int -> a -> [a]
replace xs i x = take i xs ++ [x] ++ drop (i + 1) xs

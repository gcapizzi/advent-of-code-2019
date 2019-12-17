module Main where

import System.Environment
import qualified Intcode
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified TextShow as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- TIO.readFile $ head args
  print $ findInputs 19690720 sourceCode

findInputs :: Int -> Text -> [(Int, Int)]
findInputs output sourceCode = do
    [(noun, verb) | noun <- [0..99], verb <- [0..99], runWithParams sourceCode [noun, verb] == Right output]

runWithParams :: Text -> [Int] -> Either String Int
runWithParams sourceCode params = do
    program <- parseIntList sourceCode
    let programWithParams = head program : params ++ drop (length params) (tail program)
    let sourceCodeWithParams = unparseIntList programWithParams
    (newSourceCode, _) <- Intcode.run (sourceCodeWithParams, [])
    newProgram <- parseIntList newSourceCode
    return $ head newProgram

parseInt :: Text -> Either String Int
parseInt txt = fst <$> T.decimal txt

parseIntList :: Text -> Either String [Int]
parseIntList = mapM parseInt . T.splitOn ","

unparseIntList :: [Int] -> Text
unparseIntList = T.intercalate "," . map T.showt

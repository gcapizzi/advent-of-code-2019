module Lib
    ( crackPasswords
    , isValidPassword
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified TextShow as T
import Data.List

crackPasswords :: Text -> Either String [Text]
crackPasswords textRange = do
    (left, right) <- parseRange textRange
    let guesses = map T.showt [left..right]
    return $ filter isValidPassword guesses

isValidPassword :: Text -> Bool
isValidPassword guess = hasDouble guess && hasOnlyIncreasingDigits guess

hasDouble :: Text -> Bool
hasDouble = any (hasLength 2) . groupBy (==) . T.unpack

hasLength :: Int -> [a] -> Bool
hasLength n = (== n) . length

hasOnlyIncreasingDigits :: Text -> Bool
hasOnlyIncreasingDigits = all isIncreasing . pairs . T.unpack

isDouble :: Eq a => (a, a) -> Bool
isDouble (x, y) = x == y

isIncreasing :: Ord a => (a, a) -> Bool
isIncreasing (x, y) = x <= y

parseRange :: Text -> Either String (Int, Int)
parseRange rangeTxt = do
    let [leftTxt, rightTxt] = T.splitOn "-" rangeTxt
    left <- parseInt leftTxt
    right <- parseInt rightTxt
    return (left, right)

parseInt :: Text -> Either String Int
parseInt txt = fst <$> T.decimal txt

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

module Lib
    ( calculateTotalFuel
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

calculateTotalFuel :: Text -> Either String Integer
calculateTotalFuel input = do
  masses <- readMasses input
  return $ sum $ map calculateFuel masses

readMasses :: Text -> Either String [Integer]
readMasses input = mapM parseInt (T.lines input)

calculateFuel :: Integer -> Integer
calculateFuel mass =
  if fuel > 0
    then fuel + calculateFuel fuel
    else 0
  where
    fuel = (mass `div` 3) - 2

parseInt :: Text -> Either String Integer
parseInt txt = fst <$> T.decimal txt

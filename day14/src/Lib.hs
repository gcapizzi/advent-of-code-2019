module Lib
    ( Reaction
    , Ingredient(..)
    , parseReactions
    , costOfReaction
    , reduceIngredients
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Foldable
import Data.Maybe

data Ingredient = ORE | Element String deriving (Eq, Ord, Show)
data Reaction = Reaction { element :: String, quantity :: Int, ingredients :: Map Ingredient Int } deriving (Show)
newtype ReactionBook = ReactionBook (Map String Reaction)

parseReactions :: Text -> Either String ReactionBook
parseReactions txt = do
    reactions <- mapM parseReaction $ T.lines txt
    let reactionsMap = Map.fromList $ map (\r -> (element r, r)) reactions
    return $ ReactionBook reactionsMap

parseReaction :: Text -> Either String Reaction
parseReaction txt = do
    let left:right:_ = map T.strip $ T.splitOn "=>" txt
    let qTxt:eTxt:_ = T.splitOn " " right
    let e = T.unpack eTxt
    q <- parseInt qTxt
    is <- mapM (parseIngredient . T.strip) $ T.splitOn "," left
    return Reaction { element = e, quantity = q, ingredients = Map.fromList is }

parseIngredient :: Text -> Either String (Ingredient, Int)
parseIngredient txt = do
    let qTxt:iTxt:_ = T.splitOn " " txt
    q <- parseInt qTxt
    let i = if iTxt == "ORE" then ORE else Element (T.unpack iTxt)
    return (i, q)

parseInt :: Text -> Either String Int
parseInt txt = fst <$> T.decimal txt

costOfReaction :: ReactionBook -> Int -> Ingredient -> Int
costOfReaction b q i = solution Map.! ORE
  where
    solution = head $ filter justOREs reductions
    reductions = iterate (reduceIngredients b) (Map.singleton i q)

justOREs :: Map Ingredient Int -> Bool
justOREs is = Map.size (Map.filter (>0) is) == 1 && Map.member ORE is

reduceIngredients :: ReactionBook -> Map Ingredient Int -> Map Ingredient Int
reduceIngredients b is = Map.foldr (Map.unionWith (+)) Map.empty $ Map.mapWithKey (getIngredients b) is

getIngredients :: ReactionBook -> Ingredient -> Int -> Map Ingredient Int
getIngredients b ORE q = Map.singleton ORE q
getIngredients b@(ReactionBook rs) i@(Element e) q = Map.insert i newQ $ Map.map (*n) is
  where
    r = fromMaybe (Reaction{}) $  Map.lookup e rs
    is = ingredients r
    n = q `ceiledDiv` quantity r
    newQ = q - (n * quantity r)

ceiledDiv :: Int -> Int -> Int
ceiledDiv x y = ceiling (realX / realY)
  where
    realX = fromIntegral x
    realY = fromIntegral y

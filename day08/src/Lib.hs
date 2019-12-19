module Lib
    ( parseImage
    , renderImage
    , checksum
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Ord

data Image = Image Int Int [Layer]
newtype Layer = Layer [Colour]
data Colour = Black | White | Transparent deriving (Eq)

checksum :: Image -> Int
checksum (Image _ _ layers) = numberOfWhites * numberOfTransparents
  where
    numberOfWhites = numberOf White layerWithFewestBlacks
    numberOfTransparents = numberOf Transparent layerWithFewestBlacks
    layerWithFewestBlacks = minimumBy (comparing (numberOf Black)) layers

parseImage :: Int -> Int -> Text -> Image
parseImage width height = Image width height . map parseLayer . T.chunksOf (width * height) . T.strip

parseLayer :: Text -> Layer
parseLayer = Layer . map parseColour . T.unpack

parseColour :: Char -> Colour
parseColour '0' = Black
parseColour '1' = White
parseColour '2' = Transparent

numberOf :: Colour -> Layer -> Int
numberOf colour (Layer colours) = length $ filter (== colour) colours

combineLayers :: Layer -> Layer -> Layer
combineLayers (Layer xs) (Layer ys) = Layer $ zipWith combineColours xs ys

combineColours :: Colour -> Colour -> Colour
combineColours x Transparent = x
combineColours Transparent y = y
combineColours x _ = x

renderImage :: Image -> Text
renderImage (Image width height layers) = renderLayer width combinedLayer
  where
    combinedLayer = foldr combineLayers (transparentLayer width height) layers

transparentLayer :: Int -> Int -> Layer
transparentLayer width height = Layer $ replicate (width * height) Transparent

renderLayer :: Int -> Layer -> Text
renderLayer width (Layer colours) = T.unlines $ T.chunksOf width $ T.pack $ map renderColour colours

renderColour :: Colour -> Char
renderColour Black = '0'
renderColour White = '1'
renderColour Transparent = '2'

module Solve.Day08 where

import Data.List.Split (chunksOf)
import Data.List

newtype Layer = Layer String deriving (Show, Eq)

instance Ord Layer where
  (<=) (Layer a) (Layer b) = zerosA <= zerosB
    where
      zeros = length . filter (\c -> c == '0')
      zerosA = zeros a
      zerosB = zeros b

data Pixel
  = Black
  | White
  | Transparent

toPixel :: Char -> Pixel
toPixel '0' = Black
toPixel '1' = White
toPixel '2' = Transparent
toPixel c = error "Unexpected pixel value"

instance Show Pixel where
  show Black = " "
  show White = "\9632"
  show Transparent = " "

partOne :: (Int, Int) -> String -> Int
partOne (width, height) inputStr = (ones minZeroLayer) * (twos minZeroLayer)
  where
    ones (Layer str) = length $ filter (\x -> x == '1') str
    twos (Layer str) = length $ filter (\x -> x == '2') str
    minZeroLayer = minimum $ Layer <$> chunksOf (width * height) inputStr

partTwo :: (Int, Int) -> String -> String
partTwo (width, height) inputStr = intercalate "\n" $ chunksOf width $ toString $ map getDominantColour layers
  where
    layers :: [[Pixel]]
    layers = transpose $ reverse $ chunksOf (width * height) $ map toPixel inputStr
    toString :: [Pixel] -> String
    toString = foldl (\str pixel -> str ++ show pixel) ""
    getDominantColour = foldl compare Transparent
    compare :: Pixel -> Pixel -> Pixel
    compare Transparent pixel = pixel
    compare pixel Transparent = pixel
    compare _ White = White
    compare _ Black = Black


module Helpers where

import Data.Map.Strict (Map)

parseInt :: String -> Int
parseInt a = read a :: Int

notNull :: [a] -> Bool
notNull = not . null

manhattenDistance :: (Int, Int) -> (Int, Int) -> Int
manhattenDistance (x1, y1) (x2, y2) = abs ((x1 - x2) + (y1 - y2))

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe a b = min a b

type Coordinate = (Int, Int)

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

adjacent :: Direction -> Coordinate -> Coordinate
adjacent North (x, y) = (x, y + 1)
adjacent South (x, y) = (x, y - 1)
adjacent East (x, y) = (x + 1, y)
adjacent West (x, y) = (x - 1, y)

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

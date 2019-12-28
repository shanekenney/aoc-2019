module Helpers where

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


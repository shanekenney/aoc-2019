module Helpers where

parseInt :: String -> Int
parseInt a = read a :: Int

notNull :: [a] -> Bool
notNull = not . null

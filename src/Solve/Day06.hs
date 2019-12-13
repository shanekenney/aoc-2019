module Solve.Day06 where

import Data.Tree
import Data.Set (fromList, delete, (\\), size)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Debug.Trace

parseOrbitMap :: String -> Map.Map String [String]
parseOrbitMap str = foldl updateOrbits Map.empty orbits
  where
    toTuple = (\[a, b] -> (a, b)) . splitOn ")"
    orbits = toTuple <$> lines str
    updateOrbits orbitMap (key, value) = 
      case Map.lookup key orbitMap of
        Just found -> Map.adjust (\v -> value:v) key orbitMap
        Nothing -> Map.insert key [value] orbitMap

buildOrbitTree :: Map.Map String [String] -> Tree String
buildOrbitTree orbitMap = unfoldTree findOrbiter "COM"
  where
    findChildren = Map.findWithDefault []
    findOrbiter parent = (parent, findChildren parent orbitMap)

pathToNode :: String -> Tree String -> [String]
pathToNode target (Node node []) = [node | node == target]
pathToNode target (Node node children)
  | node == target = [node]
  | otherwise = 
      case foldr find [] children of
        [] -> []
        xs -> node:xs
      where
        find node path
          | notNull path = path
          | otherwise = pathToNode target node 

notNull :: [a] -> Bool
notNull = not . null

partOne :: String -> Int
partOne inputStr = foldl countOrbits 0 orbitLevels
  where
    countOrbits total (level, objectsAtLevel) = total + (level * (length objectsAtLevel))
    orbitLevels = zip [0..] (levels $ buildOrbitTree $ parseOrbitMap inputStr)

partTwo :: String -> Int
partTwo inputStr = (size $ pathSanta \\ pathYou) + (size $ pathYou \\ pathSanta)
  where
    (you, santa) = ("YOU", "SAN")
    orbitTree = buildOrbitTree $ parseOrbitMap inputStr
    pathSanta = delete santa $ fromList $ pathToNode santa orbitTree
    pathYou = delete you $ fromList $ pathToNode you orbitTree

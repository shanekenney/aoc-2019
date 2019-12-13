module Solve.Day03 where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Helpers

data Move
  = MoveLeft Int
  | MoveRight Int
  | MoveUp Int
  | MoveDown Int 
  deriving Show

parseMove :: String -> Move
parseMove ('L':value) = MoveLeft $ parseInt value
parseMove ('R':value) = MoveRight $ parseInt value
parseMove ('U':value) = MoveUp $ parseInt value
parseMove ('D':value) = MoveDown $ parseInt value

parseMoves :: String -> [[Move]]
parseMoves str = [parse line | line <- lines str]
  where parse line = parseMove <$> splitOn "," line

move :: (Int, Int) -> Move -> [(Int, Int)]
move (start, y) (MoveLeft n) = [(x,y) | x <- range]
  where
    range = [start-n..start-1]

move (start, y) (MoveRight n) = [(x,y) | x <- range]
  where
    range = [start+n, start+n-1..start+1]

move (x, start) (MoveUp n) = [(x,y) | y <- range]
  where
    range = [start+n, start+n-1..start+1]

move (x, start) (MoveDown n) = [(x,y) | y <- range]
  where
    range = [start-n..start-1]

wire :: [Move] -> [(Int, Int)]
wire moves = foldl add [(0,0)] moves
  where add (start:xs) m = (move start m) ++ start:xs

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = x + y
  where
    x = abs (x1 - x2)
    y = abs (y1 - y2)

partOne :: String -> Int
partOne inputStr = minimum $ distanceFromOrigin <$> intersections
  where
    distanceFromOrigin = distance (0,0)
    intersections = filter (\pos -> pos /= (0,0)) $ Set.toList $ Set.intersection wire1 wire2
    (moves1:moves2:[]) = parseMoves inputStr
    wire1 = Set.fromList $ wire moves1
    wire2 = Set.fromList $ wire moves2

partTwo :: String -> Int
partTwo inputStr = minimum $ (stepsToIntersection (reverse wire1) (reverse wire2)) <$> intersections
  where
    intersections = filter (\pos -> pos /= (0,0)) $
        Set.toList $
        Set.intersection (Set.fromList wire1) (Set.fromList wire2)
    (moves1:moves2:[]) = parseMoves inputStr
    stepsToIntersection wire1 wire2 intersection =
      sum $ catMaybes [(elemIndex intersection $ wire1), (elemIndex intersection $ wire2)]
    wire1 = wire moves1
    wire2 = wire moves2

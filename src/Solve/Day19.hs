module Solve.Day19 where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import IntCode
import Debug.Trace (traceShow)

input :: String
input = "109,424,203,1,21102,1,11,0,1106,0,282,21101,18,0,0,1105,1,259,1201,1,0,221,203,1,21102,1,31,0,1105,1,282,21102,38,1,0,1106,0,259,20102,1,23,2,21201,1,0,3,21101,1,0,1,21101,0,57,0,1106,0,303,1202,1,1,222,20101,0,221,3,21001,221,0,2,21101,259,0,1,21102,80,1,0,1106,0,225,21102,1,145,2,21101,91,0,0,1105,1,303,2101,0,1,223,20101,0,222,4,21102,1,259,3,21101,0,225,2,21101,0,225,1,21102,1,118,0,1105,1,225,21001,222,0,3,21101,80,0,2,21101,133,0,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21101,148,0,0,1106,0,259,1201,1,0,223,20102,1,221,4,20101,0,222,3,21101,0,23,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21102,1,195,0,106,0,109,20207,1,223,2,21001,23,0,1,21102,1,-1,3,21102,1,214,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,2101,0,-4,249,22101,0,-3,1,22102,1,-2,2,21201,-1,0,3,21101,0,250,0,1105,1,225,21202,1,1,-4,109,-5,2105,1,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2106,0,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22101,0,-2,-2,109,-3,2105,1,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21202,-2,1,3,21102,1,343,0,1106,0,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,22101,0,-4,1,21102,384,1,0,1105,1,303,1106,0,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21202,1,1,-4,109,-5,2106,0,0"

data Space
  = Empty
  | Beam
  deriving (Show, Eq)

spaceChar :: Space -> Char
spaceChar Empty = '.'
spaceChar Beam = '#'

toSpace :: Int -> Space
toSpace 0 = Empty
toSpace 1 = Beam
toSpace n = error $ show n ++ " isn't a valid space value."

emptyGrid :: Int -> [(Int,Int)]
emptyGrid size = [(x, y) | y <- [0..(size - 1)], x <- [0..(size - 1)]]

getBeamMap :: String -> [(Int, Int)] -> [((Int, Int), Space)]
getBeamMap intcode grid = getBeam <$> grid
  where
    memory = initMemory $ programVector intcode
    getBeam (x, y) = ((x, y), space)
      where
        space = toSpace $ head $ outputs $ execute memory { inputs = [x, y] } 

partOne :: String -> Int
partOne inputStr = length $ filter beam $ getBeamMap inputStr $ emptyGrid 50
  where
    beam (_, space) = space == Beam

display :: [((Int, Int), Space)] -> IO ()
display grid = putStrLn $ intercalate "\n" $ chunksOf width $ toChars <$> grid
  where
    toChars (_, space) = spaceChar space
    width = (fst $ fst $ last grid) + 1

partTwo inputStr squareWidth = last $ filter beam $ getBeamMap inputStr [(x, y) | y <- [minY], x <- [0..minY]]
  where
    minY = findMinFit 0 1500 comparer
    beam (_, space) = space == Beam
    comparer y'
      | xll < (xur - squareDiff) = GT -- too big
      | xll > (xur - squareDiff) = LT -- too small
      | otherwise = EQ
      where
        squareDiff = squareWidth - 1
        ((xur, _), _) = last $ filter beam $ getBeamMap inputStr [(x, y) | y <- [y'], x <- [0..y']]
        ((xll, _), _) = head $ filter beam $ getBeamMap inputStr [(x, y) | y <- [y' + squareDiff], x <- [0..y' + squareDiff]]

findMinFit :: Int -> Int -> (Int -> Ordering) -> Int
findMinFit lower upper comparer
  | lower == upper = lower
  | result == EQ = mid
  | otherwise = if result == GT then findMinFit lower (mid - 1) comparer else findMinFit (mid + 1) upper comparer
  where 
    mid = ((traceShow lower lower) + (traceShow upper upper)) `div` 2
    result = comparer mid

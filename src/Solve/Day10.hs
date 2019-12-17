module Solve.Day10 where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Control.Arrow
import Helpers

testcase = ".#..#\n.....\n#####\n....#\n...##\n"
testcase1 = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
input = "...###.#########.####\n.######.###.###.##...\n####.########.#####.#\n########.####.##.###.\n####..#.####.#.#.##..\n#.################.##\n..######.##.##.#####.\n#.####.#####.###.#.##\n#####.#########.#####\n#####.##..##..#.#####\n##.######....########\n.#######.#.#########.\n.#.##.#.#.#.##.###.##\n######...####.#.#.###\n###############.#.###\n#.#####.##..###.##.#.\n##..##..###.#.#######\n#..#..########.#.##..\n#.#.######.##.##...##\n.#.##.#####.#..#####.\n#.#.##########..#.##."

partOne :: String -> ((Int, Int), Int)
partOne = parseInput
  >>> anglesFromOriginToPoint
  >>> toCoordinateMap
  >>> orderByDistance
  >>> getClosestDistance
  >>> countVisible
  >>> findMax

type Coordinate = (Int, Int)

partTwo :: String -> (Double, Coordinate)
partTwo inputStr = 
  case Map.lookup (11,13) coordinateMap of
    Just angles -> (Map.toList $ Map.map (\points -> head points) angles) !! 199
    Nothing -> error "Should exist"
  where coordinateMap = parseInput
          >>> anglesFromOriginToPoint
          >>> toCoordinateMap
          >>> orderByDistance $ inputStr

angleDegrees :: (Int, Int) -> (Int, Int) -> Double
angleDegrees (x1, y1) (x2, y2)
  | atan' < 0 = angle + 360
  | otherwise = angle
  where atan' = atan2 (fromIntegral (x2 - x1)) (fromIntegral (y1 - y2))
        angle = atan' * 180 / pi

parseInput :: String -> [(Int, Int)]
parseInput inputStr = concat $ pairCoordinates <$> zip xxs ys
  where
    rows = lines inputStr
    asteroidIndex = elemIndices '#'
    xxs = asteroidIndex <$> rows
    ys = take (length rows) [0..]
    pairCoordinates (xs, y) = map (\x -> (x,y)) xs

anglesFromOriginToPoint :: [Coordinate] -> [(Coordinate, (Double, Coordinate))]
anglesFromOriginToPoint coordinates = 
  [(origin, (angle, point)) | origin <- coordinates, 
                              point <- coordinates, 
                              let angle = angleDegrees origin point, 
                              origin /= point]

toCoordinateMap :: [(Coordinate, (Double, Coordinate))] -> Map Coordinate (Map Double [Coordinate])
toCoordinateMap anglesFromOrigin = foldl update Map.empty anglesFromOrigin
  where
    update originMap (originCoord, (angle, point)) = 
      case Map.lookup originCoord originMap of
        Just _ -> Map.adjust (\points -> updatePointsAtAngle (angle, point) points) originCoord originMap
        Nothing -> Map.insert originCoord (Map.singleton angle [point]) originMap

    updatePointsAtAngle :: (Double, Coordinate) -> (Map Double [Coordinate]) -> (Map Double [Coordinate])
    updatePointsAtAngle (angle, point) pointsAtAngle = 
      case Map.lookup angle pointsAtAngle of
        Just _ -> Map.adjust (\existing -> point:existing) angle pointsAtAngle
        Nothing -> Map.insert angle [point] pointsAtAngle

orderByDistance :: Map Coordinate (Map Double [Coordinate]) -> Map Coordinate (Map Double [Coordinate])
orderByDistance anglesFromOrigin = Map.mapWithKey sortPointsByDistance anglesFromOrigin
  where
    sortPointsByDistance origin angles = Map.map (\points -> sortBy (distance origin) points) angles
    distance origin pointA pointB = 
      let distFromOrigin = manhattenDistance origin
      in compare (distFromOrigin pointA) (distFromOrigin pointB)

getClosestDistance :: Map Coordinate (Map Double [Coordinate]) -> Map Coordinate (Map Double Int)
getClosestDistance anglesFromOrigin = Map.mapWithKey sortPointsByDistance anglesFromOrigin
  where
    sortPointsByDistance origin angles = Map.map (\points -> manhattenDistance origin $ head $ points) angles

countVisible :: Map Coordinate (Map Double Int) -> Map Coordinate Int
countVisible coordMap = Map.map Map.size coordMap

findMax :: (Map (Int, Int) Int) -> ((Int,Int), Int)
findMax pointMap = foldl setMax ((0,0), 0) $ Map.toList pointMap
  where
    setMax (maxCoord, maxCount) (coord, count)
      | count > maxCount = (coord, count)
      | otherwise = (maxCoord, maxCount)

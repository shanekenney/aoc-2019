module Solve.Day01 where

import Helpers

fuel :: Int -> Int
fuel mass = (mass `div` 3) - 2

fuelWithFuel :: Int -> Int
fuelWithFuel mass = fuelForFuel' (fuel mass)
  where 
    fuelForFuel' m = 
      let f = fuel m 
      in if f > 0 then m + (fuelForFuel' f) else m

partOne :: [String] -> Int
partOne lines = sum $ map fuel masses 
  where
    masses = map parseInt lines

partTwo :: [String] -> Int
partTwo lines = sum $ map fuelWithFuel masses
  where 
    masses = map parseInt lines


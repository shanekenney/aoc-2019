module Solve.Day05 where

import Data.Vector as V (fromList, slice, (!), (//), Vector, head, toList)
import Data.List.Split (splitOn)
import IntCode
import Helpers

generate :: String -> IntCodeMemory
generate inputStr = execute memory { inputs = inputs }
  where
    inputs = [5]
    instructionVector = fromList $ parseInt <$> splitOn "," inputStr
    memory = initMemory instructionVector


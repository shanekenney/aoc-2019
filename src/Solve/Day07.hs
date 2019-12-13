module Solve.Day07 where

import Data.List
import Data.List.Split (splitOn)
import Data.Vector (Vector)
import IntCode
import Helpers
import Debug.Trace (trace)


partOne :: String -> Int
partOne input = maximum $ map run $ permutations [0..4]
  where
    amplifier = initMemory $ programVector input
    run = foldl executeAmplifier 0
    executeAmplifier phase input = head $ outputs $ execute $ amplifier { inputs = [input, phase]}

partTwo :: String -> Int
partTwo input = maximum $ map (\phases -> run (amplifiers phases) 0) $ permutations [5..9]
  where
    amplifier = initMemory $ programVector input
    amplifiers phases = [amplifier { inputs = [phase] } | phase <- phases]

    run :: [IntCodeMemory] -> Int -> Int
    run (amp:amplifiers) input = 
      case newMemory of
        (IntCodeMemory 99 _ _ _ (output: _)) ->
          case amplifiers of 
            [] -> output
            xs -> run amplifiers output
        (IntCodeMemory _ _ _ _ (output:_)) -> run newAmplifiers output
      where
        (IntCodeMemory _ _ _ inputs _) = amp
        ampWithInput = trace ("in: " ++ show (amp { inputs = inputs ++ [input] })) amp { inputs = inputs ++ [input] }
        newMemory = trace ("out: " ++ show (execute ampWithInput)) (execute ampWithInput)
        newAmplifiers = amplifiers ++ [newMemory { outputs = [] }]


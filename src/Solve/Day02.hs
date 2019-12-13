module Solve.Day02 where

import Data.Vector as V (fromList, slice, (!), (//), Vector, head)
import Data.List.Split (splitOn)
import Helpers

vectorOperation :: (Int -> Int -> Int) -> Int -> Int -> Int -> Vector Int -> Vector Int
vectorOperation op inPosA inPosB outPos vector = vector // [(outPos, value)]
  where
    operandA = vector ! inPosA
    operandB = vector ! inPosB
    value = op operandA operandB

add = vectorOperation (+)
multiply = vectorOperation (*)

processInstruction :: Int -> Int -> Vector Int -> Vector Int
processInstruction 1 instructionPointer vector = processInstruction nextOpCode nextPointer nextVector
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    nextVector = add inPosA inPosB outPos vector

processInstruction 2 instructionPointer vector = processInstruction nextOpCode nextPointer nextVector
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    nextVector = multiply inPosA inPosB outPos vector

processInstruction 99 _ vector = vector

inputStr :: String
inputStr = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0"

partOne :: Vector Int -> Int
partOne input = V.head $ processInstruction opCode instructionPointer instructionVector
  where
    instructionPointer = 0
    instructionVector = input
    opCode = instructionVector ! instructionPointer

partTwo :: Int -> Int -> Int
partTwo noun verb = partOne (instructionVector // [(1, noun), (2, verb)])
  where
    instructionVector = fromList $ parseInt <$> splitOn "," inputStr


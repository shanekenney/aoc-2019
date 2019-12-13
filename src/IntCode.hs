module IntCode (execute, programVector, IntCodeMemory(..), initMemory) where

import Data.Vector as V (fromList, slice, (!), (//), Vector, head, toList)
import Data.List.Split (splitOn)
import Helpers

data IntCodeMemory = IntCodeMemory
  { opCode :: Int
  , instructionPointer :: Int
  , vector :: Vector Int
  , inputs :: [Int]
  , outputs :: [Int]
  } deriving (Show)


vectorOperation :: (Int -> Int -> Int) -> Int -> Int -> Int -> Vector Int -> Vector Int
vectorOperation op inPosA inPosB outPos vector = vector // [(outPos, value)]
  where
    operandA = vector ! inPosA
    operandB = vector ! inPosB
    value = op operandA operandB

add = vectorOperation (+)
multiply = vectorOperation (*)
less x y = if x < y then 1 else 0
equals x y = if x == y then 1 else 0

vecLess = vectorOperation less
vecEqual = vectorOperation equals

processInstruction :: IntCodeMemory -> IntCodeMemory
processInstruction (IntCodeMemory 1 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextVector = add inPosA inPosB outPos vector
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer

processInstruction (IntCodeMemory 101 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = vector ! inPosB
    value = operandA + operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1001 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = vector ! inPosA
    operandB = inPosB
    value = operandA + operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1101 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = inPosB
    value = operandA + operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 2 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    nextVector = multiply inPosA inPosB outPos vector

processInstruction (IntCodeMemory 102 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = vector ! inPosB
    value = operandA * operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1002 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = vector ! inPosA
    operandB = inPosB
    value = operandA * operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1102 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = inPosB
    value = operandA * operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 7 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    nextVector = vecLess inPosA inPosB outPos vector

processInstruction (IntCodeMemory 107 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = vector ! inPosB
    value = operandA `less` operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1007 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = vector ! inPosA
    operandB = inPosB
    value = operandA `less` operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1107 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = inPosB
    value = operandA `less` operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 8 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    nextVector = vecEqual inPosA inPosB outPos vector

processInstruction (IntCodeMemory 108 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = vector ! inPosB
    value = operandA `equals` operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1008 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = vector ! inPosA
    operandB = inPosB
    value = operandA `equals` operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 1108 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inPosA = vector ! (instructionPointer + 1)
    inPosB = vector ! (instructionPointer + 2)
    outPos = vector ! (instructionPointer + 3)
    nextPointer = instructionPointer + 4
    nextOpCode = vector ! nextPointer
    operandA = inPosA
    operandB = inPosB
    value = operandA `equals` operandB
    nextVector = vector // [(outPos, value)]

processInstruction (IntCodeMemory 99 instructionPointer vector inputs outputs) = (IntCodeMemory 99 0 vector [] inputs)

processInstruction (IntCodeMemory 3 instructionPointer vector (input:inputs) outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    outPos = vector ! (instructionPointer + 1)
    nextPointer = instructionPointer + 2
    nextOpCode = vector ! nextPointer
    nextVector = vector // [(outPos, input)]

--processInstruction (IntCodeMemory 4 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs newoutputs)
  --where
    --outValue = vector ! (vector ! (instructionPointer + 1))
    --newoutputs) = outValue:outputs
    --nextPointer = instructionPointer + 2
    --nextOpCode = vector ! nextPointer
    --nextVector = vector

--processInstruction (IntCodeMemory 104 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs newOutputs)
  --where
    --outValue = vector ! (instructionPointer + 1)
    --newoutputs) = outValue:outputs
    --nextPointer = instructionPointer + 2
    --nextOpCode = vector ! nextPointer
    --nextVector = vector

processInstruction (IntCodeMemory 4 instructionPointer vector inputs outputs) = (IntCodeMemory nextOpCode nextPointer nextVector inputs newOutputs)
  where
    outValue = vector ! (vector ! (instructionPointer + 1))
    newOutputs = outValue:outputs
    nextPointer = instructionPointer + 2
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 104 instructionPointer vector inputs outputs) = (IntCodeMemory nextOpCode nextPointer nextVector inputs newOutputs)
  where
    outValue = vector ! (instructionPointer + 1)
    newOutputs = outValue:outputs
    nextPointer = instructionPointer + 2
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 5 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (vector ! (instructionPointer + 1))
    outValue = vector ! (instructionPointer + 2)
    nextPointer = if inValue /= 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 105 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (instructionPointer + 1)
    outValue = vector ! (vector ! (instructionPointer + 2))
    nextPointer = if inValue /= 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 1005 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (vector ! (instructionPointer + 1))
    outValue = vector ! (instructionPointer + 2)
    nextPointer = if inValue /= 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 1105 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (instructionPointer + 1)
    outValue = vector ! (instructionPointer + 2)
    nextPointer = if inValue /= 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 6 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (vector ! (instructionPointer + 1))
    outValue = vector ! (vector ! (instructionPointer + 2))
    nextPointer = if inValue == 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 106 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (instructionPointer + 1)
    outValue = vector ! (vector ! (instructionPointer + 2))
    nextPointer = if inValue == 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 1006 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (vector ! (instructionPointer + 1))
    outValue = vector ! (instructionPointer + 2)
    nextPointer = if inValue == 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory 1106 instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory nextOpCode nextPointer nextVector inputs outputs)
  where
    inValue = vector ! (instructionPointer + 1)
    outValue = vector ! (instructionPointer + 2)
    nextPointer = if inValue == 0 then outValue else instructionPointer + 3
    nextOpCode = vector ! nextPointer
    nextVector = vector

processInstruction (IntCodeMemory n instructionPointer vector inputs outputs) = processInstruction (IntCodeMemory (vector ! instructionPointer) instructionPointer vector inputs outputs)

execute :: IntCodeMemory -> IntCodeMemory
execute memory = processInstruction memory

initMemory :: Vector Int -> IntCodeMemory
initMemory vector = IntCodeMemory 
  { opCode = opCode
  , instructionPointer = instructionPointer
  , vector = vector
  , inputs = []
  , outputs = []
  }
  where
    instructionPointer = 0
    opCode = vector ! instructionPointer

programVector :: String -> Vector Int
programVector = fromList . map parseInt . splitOn ","


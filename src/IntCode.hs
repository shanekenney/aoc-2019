module IntCode (execute, programVector, IntCodeMemory(..), initMemory) where

import Data.Vector as V (fromList, slice, (!), (//), Vector, head, toList)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.List.Split (splitOn)
import Helpers
import Text.Printf
import Debug.Trace (traceShow)

data IntCodeMemory = IntCodeMemory
  { instructionPointer :: Int
  , relativeBase :: Int
  , vector :: Vector Int
  , inputs :: [Int]
  , outputs :: [Int]
  } deriving (Show)

data OpCode
  = Add 
  | Multiply   
  | LessThan   
  | Equals   
  | Halt
  | Input 
  | Output 
  | JumpTrue  
  | JumpFalse
  deriving (Show)

data ParameterMode
  = Position
  | Immediate
  | Relative
  deriving (Show)

data ParameterPosition
  = First
  | Second
  | Third
  deriving (Show)

data ParameterType
  = Value
  | Address

data Operation = Operation
  { code :: OpCode
  , paramMode :: (ParameterMode, ParameterMode, ParameterMode)
  } deriving (Show)

toOpCode :: Int -> OpCode
toOpCode 1 = Add
toOpCode 2 = Multiply
toOpCode 3 = Input
toOpCode 4 = Output
toOpCode 5 = JumpTrue
toOpCode 6 = JumpFalse
toOpCode 7 = LessThan
toOpCode 8 = Equals
toOpCode 99 = Halt

toParameterMode :: Int -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate
toParameterMode 2 = Relative

parseOperation :: Int -> Operation
parseOperation code = Operation 
  { code = toOpCode $ intCode
  , paramMode = (param1, param2, param3)
  }
  where
    opCodeStr = printf "%05d" code
    intCode = parseInt $ drop 3 $ opCodeStr
    parseParameterMode = toParameterMode . parseInt . (:[]) . (!!) opCodeStr
    param1 = parseParameterMode 2
    param2 = parseParameterMode 1
    param3 = parseParameterMode 0

readParam :: IntCodeMemory -> (ParameterMode, ParameterMode, ParameterMode) -> ParameterType -> ParameterPosition -> Int
readParam memory modes paramType position = 
  case mode of
    Position -> case paramType of 
                  Address -> paramValue
                  Value -> vect ! paramValue
    Immediate -> paramValue
    Relative -> case paramType of 
                  Address -> paramValue
                  Value -> vect ! ((relativeBase memory) + paramValue)
  where
    vect = vector memory
    paramValue = vect ! ((instructionPointer memory) + offset)
    (mode, offset) = case position of
                       First -> (sel1 modes, 1)
                       Second -> (sel2 modes, 2)
                       Third -> (sel3 modes, 3)

perform :: Operation -> IntCodeMemory -> IntCodeMemory
perform (Operation Add paramMode) memory = memory 
  { vector = (vector memory) // [(writeAddress, value)]
  , instructionPointer = (instructionPointer memory) + 4
  }
  where
    readParam' = readParam memory paramMode
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third
    value = x + y

perform (Operation Multiply paramMode) memory = memory 
  { vector = (vector memory) // [(writeAddress, value)]
  , instructionPointer = (instructionPointer memory) + 4
  }
  where
    readParam' = readParam memory paramMode
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third
    value = x * y

perform (Operation LessThan paramMode) memory = memory 
  { vector = (vector memory) // [(writeAddress, value)]
  , instructionPointer = (instructionPointer memory) + 4
  }
  where
    readParam' = readParam memory paramMode
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third
    value = if x < y then 1 else 0

perform (Operation Equals paramMode) memory = memory 
  { vector = (vector memory) // [(writeAddress, value)]
  , instructionPointer = (instructionPointer memory) + 4
  }
  where
    readParam' = readParam memory paramMode
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third
    value = if x == y then 1 else 0

perform (Operation Input paramMode) memory = memory 
  { vector = (vector memory) // [(writeAddress, x)]
  , instructionPointer = (instructionPointer memory) + 2
  , inputs = xs
  }
  where
    readParam' = readParam memory paramMode
    writeAddress = readParam' Address First
    (x:xs) = inputs memory

perform (Operation Output paramMode) memory = memory 
  { instructionPointer = (instructionPointer memory) + 2
  , outputs = x:(outputs memory)
  }
  where
    readParam' = readParam memory paramMode
    x = readParam' Value First

perform (Operation JumpTrue paramMode) memory = memory 
  { instructionPointer = nextPointer }
  where
    readParam' = readParam memory paramMode
    x = readParam' Value First
    jumpAddress = readParam' Value Second
    nextPointer = if x /= 0 then jumpAddress else (instructionPointer memory) + 3

perform (Operation JumpFalse paramMode) memory = memory 
  { instructionPointer = nextPointer }
  where
    readParam' = readParam memory paramMode
    x = readParam' Value First
    jumpAddress = readParam' Value Second
    nextPointer = if x == 0 then jumpAddress else (instructionPointer memory) + 3

perform (Operation Halt _) memory = memory
  { instructionPointer = -1
  , inputs = []
  , outputs = inputs memory 
  }

processInstruction :: IntCodeMemory -> IntCodeMemory
processInstruction memory = if continue then processInstruction newMemory else newMemory 
  where
    opcodeInt = (vector memory) ! instructionPointer memory
    --operation = traceShow (parseOperation opcodeInt) (parseOperation opcodeInt)
    operation = parseOperation opcodeInt
    newMemory = perform operation memory
    continue = case operation of
                 (Operation Output _) -> False
                 (Operation Halt _) -> False
                 other -> True

execute :: IntCodeMemory -> IntCodeMemory
execute memory = processInstruction memory

initMemory :: Vector Int -> IntCodeMemory
initMemory vector = IntCodeMemory 
  { instructionPointer = 0
  , relativeBase = 0
  , vector = vector
  , inputs = []
  , outputs = []
  }

programVector :: String -> Vector Int
programVector = fromList . map parseInt . splitOn ","


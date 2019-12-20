module IntCode (execute, programVector, IntCodeMemory(..), initMemory, halted) where

import Data.Vector (Vector, fromList, (!), (//))
import qualified Data.Vector as V ((++), replicate)
import Data.Tuple.Select (sel1, sel2, sel3)
import Data.List.Split (splitOn)
import Helpers
import Text.Printf
import Data.Either
--import Debug.Trace (traceShow)

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
  | AdjustRelativeBase
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
toOpCode 9 = AdjustRelativeBase
toOpCode 99 = Halt
toOpCode n = error $ show n ++ " is not a valid opcode."

toParameterMode :: Int -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate
toParameterMode 2 = Relative
toParameterMode n = error $ show n  ++ " is not a valid parameter mode."

parseOperation :: Int -> Operation
parseOperation opcode = Operation 
  { code = toOpCode $ intCode
  , paramMode = (param1, param2, param3)
  }
  where
    opCodeStr = printf "%05d" opcode
    intCode = parseInt $ drop 3 $ opCodeStr
    parseParameterMode = toParameterMode . parseInt . (:[]) . (!!) opCodeStr
    param1 = parseParameterMode 2
    param2 = parseParameterMode 1
    param3 = parseParameterMode 0

checkBounds :: Vector Int -> Int -> Int -> Either Int Int
checkBounds vect index right = if length vect > index then Right right else Left index

resize :: IntCodeMemory -> [Either Int Int] -> Either IntCodeMemory [Int]
resize memory eithers =
  if notNull outOfBounds then Left memory { vector = resizedVect } else Right (rights eithers)
  where
    vect = vector memory
    outOfBounds = lefts eithers
    size = (maximum outOfBounds) - (length vect) + 1
    resizedVect = vect V.++ (V.replicate size 0)

readParam :: IntCodeMemory -> (ParameterMode, ParameterMode, ParameterMode) -> ParameterType -> ParameterPosition -> Either Int Int
readParam memory modes paramType position = 
  case mode of
    Immediate -> Right paramValue
    Position -> case paramType of 
                  Address -> check paramValue paramValue
                  Value -> check paramValue (vect ! paramValue)
    Relative -> case paramType of 
                  Address -> check ((relativeBase memory) + paramValue) ((relativeBase memory) + paramValue)
                  Value -> check ((relativeBase memory) + paramValue) (vect ! ((relativeBase memory) + paramValue))
  where
    vect = vector memory
    check = checkBounds vect
    paramValue = vect ! ((instructionPointer memory) + offset)
    (mode, offset) = case position of
                       First -> (sel1 modes, 1)
                       Second -> (sel2 modes, 2)
                       Third -> (sel3 modes, 3)

perform :: Operation -> IntCodeMemory -> IntCodeMemory
perform (Operation Add paramMode') memory =
  case resize memory [x, y, writeAddress] of
    Left resizedMemory -> resizedMemory
    Right [x', y', writeAddress'] -> memory 
      { vector = (vector memory) // [(writeAddress', (x' + y'))]
      , instructionPointer = (instructionPointer memory) + 4
      }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third

perform (Operation Multiply paramMode') memory =
  case resize memory [x, y, writeAddress] of
    Left resizedMemory -> resizedMemory
    Right [x', y', writeAddress'] -> memory
      { vector = (vector memory) // [(writeAddress', x' * y')]
      , instructionPointer = (instructionPointer memory) + 4
      }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third

perform (Operation LessThan paramMode') memory =
  case resize memory [x, y, writeAddress] of
    Left resizedMemory -> resizedMemory
    Right [x', y', writeAddress'] ->
      let value = if x' < y' then 1 else 0
      in memory
        { vector = (vector memory) // [(writeAddress', value)]
        , instructionPointer = (instructionPointer memory) + 4
        }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third

perform (Operation Equals paramMode') memory =
  case resize memory [x, y, writeAddress] of
    Left resizedMemory -> resizedMemory
    Right [x', y', writeAddress'] ->
      let value = if x' == y' then 1 else 0
      in memory
        { vector = (vector memory) // [(writeAddress', value)]
        , instructionPointer = (instructionPointer memory) + 4
        }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First
    y = readParam' Value Second
    writeAddress = readParam' Address Third

perform (Operation Input paramMode') memory =
  case resize memory [writeAddress] of
    Left resizedMemory -> resizedMemory
    Right [writeAddress'] -> memory
      { vector = (vector memory) // [(writeAddress', x)]
      , instructionPointer = (instructionPointer memory) + 2
      , inputs = xs
      }
  where
    readParam' = readParam memory paramMode'
    writeAddress = readParam' Address First
    (x:xs) = inputs memory

perform (Operation Output paramMode') memory =
  case resize memory [x] of
    Left resizedMemory -> resizedMemory
    Right [x'] -> memory
      { instructionPointer = (instructionPointer memory) + 2
      , outputs = x':(outputs memory)
      }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First

perform (Operation JumpTrue paramMode') memory =
  case resize memory [x, jumpAddress] of
    Left resizedMemory -> resizedMemory
    Right [x', jumpAddress'] -> 
      let nextPointer = if x' /= 0 then jumpAddress' else (instructionPointer memory) + 3
      in memory { instructionPointer = nextPointer }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First
    jumpAddress = readParam' Value Second

perform (Operation JumpFalse paramMode') memory =
  case resize memory [x, jumpAddress] of
    Left resizedMemory -> resizedMemory
    Right [x', jumpAddress'] -> 
      let nextPointer = if x' == 0 then jumpAddress' else (instructionPointer memory) + 3
      in memory { instructionPointer = nextPointer }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First
    jumpAddress = readParam' Value Second

perform (Operation AdjustRelativeBase paramMode') memory =
  case resize memory [x] of
    Left resizedMemory -> resizedMemory
    Right [x'] -> 
      let newRelativeBase = (relativeBase memory) + x'
      in memory
      { instructionPointer = (instructionPointer memory) + 2 
      , relativeBase = newRelativeBase 
      }
  where
    readParam' = readParam memory paramMode'
    x = readParam' Value First

perform (Operation Halt _) memory = memory
  { instructionPointer = -1
  , inputs = []
  , outputs = []
  }

processInstruction :: IntCodeMemory -> IntCodeMemory
processInstruction memory@(IntCodeMemory { instructionPointer = -1}) = memory 
processInstruction memory = if continue then processInstruction newMemory else newMemory 
  where
    opcodeInt = (vector memory) ! instructionPointer memory
    operation = parseOperation opcodeInt
    newMemory = perform operation memory
    continue = case operation of
                 (Operation Output _) -> False
                 (Operation Halt _) -> False
                 _ -> True

execute :: IntCodeMemory -> IntCodeMemory
execute memory = processInstruction memory

initMemory :: Vector Int -> IntCodeMemory
initMemory vec = IntCodeMemory 
  { instructionPointer = 0
  , relativeBase = 0
  , vector = vec
  , inputs = []
  , outputs = []
  }

programVector :: String -> Vector Int
programVector = fromList . map parseInt . splitOn ","

halted :: IntCodeMemory -> Bool
halted IntCodeMemory{ instructionPointer = -1 } = True
halted _ = False


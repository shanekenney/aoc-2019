module Solve.Day14 where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, upper, spaces, string, newline)
import Text.Parsec (many1, sepBy)
import Text.Parsec.Prim (parse)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

input :: String
input = "1 FJFL, 1 BPVQN => 7 CMNH\n6 FJFL, 2 KZJLT, 3 DZQJ => 2 NSPZ\n11 TPZDN => 2 TNMC\n1 NSPZ, 2 KQVL => 2 HPNWP\n3 XHDVT => 9 LRBN\n3 LRBN => 6 TPZDN\n1 KPFLZ, 1 XVXCZ => 6 WHMLV\n1 BDWQP, 1 JPGK, 1 MTWG => 5 GLHWQ\n2 BGLTP, 1 HPNWP, 2 GLHWQ, 9 CRJZ, 22 QVQJ, 3 PHGWC, 1 BDWQP => 3 LKPNB\n4 BDSB => 2 PNSD\n2 BRJDJ, 13 THQR => 2 BGLTP\n1 WHJKH, 2 JBTJ => 6 THQR\n1 JBTJ => 9 WGVP\n10 CTXHZ, 2 DGMN => 5 TNVC\n7 LCSV, 1 LKPNB, 36 CMNH, 1 JZXPH, 20 DGJPN, 3 WDWB, 69 DXJKC, 3 WHJKH, 18 XSGP, 22 CGZL, 2 BNVB, 57 PNSD => 1 FUEL\n13 CRCG, 8 NMQN => 1 JZXPH\n2 FZVS, 2 ZPFBH => 9 SRPD\n1 QPNTQ, 4 QVQJ, 1 XZKTG => 9 WDWB\n6 SXZW => 5 FJFL\n6 GVGZ => 6 ZPFBH\n1 JPGK, 3 WDFXH, 22 FJFL => 7 BDSB\n3 WHMLV => 4 JPGK\n7 CGZL, 4 LRBN => 8 MTWG\n11 SXZW, 33 ZTBFN => 4 XVXCZ\n1 FZVS, 1 TNMC, 7 JPGK => 9 FLHW\n2 XKFZ => 8 CGZL\n5 WHMLV => 8 MQRS\n1 QVSH, 6 TPZDN, 9 JQHCH => 2 BMNJ\n3 CMNH, 10 XKFZ => 2 KQVL\n119 ORE => 9 PSPQ\n1 WGVP, 18 BRJDJ => 9 PHGWC\n110 ORE => 6 NMQN\n13 NMQN, 24 XVXCZ, 9 XHDVT => 6 KQVS\n6 TNMC => 4 DXJKC\n1 XZKTG => 8 WHJKH\n1 KPFLZ, 1 LRBN, 7 KQVS => 9 JBTJ\n1 XKFZ => 8 JVGD\n152 ORE => 7 SXZW\n1 BDWQP => 5 CTXHZ\n2 JVGD, 8 DGMN, 2 MTWG => 6 QVQJ\n1 KQVL => 2 BNVB\n3 DZQJ, 37 MQRS => 4 CRJZ\n1 KQVL, 5 WDFXH => 7 BDWQP\n3 GVGZ => 3 BPVQN\n4 PSPQ, 6 ZTBFN => 1 KPFLZ\n34 FBTG => 9 XZKTG\n14 TNMC, 4 FZVS, 3 MTWG => 9 KZJLT\n157 ORE => 6 GVGZ\n5 JVGD, 11 JPGK => 5 CRCG\n1 SXZW, 1 NMQN => 3 XHDVT\n1 FBTG => 5 JQHCH\n3 WDFXH, 4 FZVS, 9 CGFML => 6 DZQJ\n5 BDWQP, 3 TNVC, 7 SRPD, 1 WDFXH, 3 JQHCH, 4 QVQJ, 5 CRCG, 4 DGMN => 7 XSGP\n1 KPFLZ, 3 TPZDN, 1 SRPD => 6 FBTG\n1 WHMLV, 3 BDSB, 2 JVGD => 9 LCSV\n13 XZKTG => 4 QVSH\n1 XHDVT => 7 XKFZ\n1 CMNH, 1 KQVS, 2 XVXCZ => 6 CGFML\n6 FLHW => 4 BRJDJ\n2 KQVL, 2 WGVP, 7 BMNJ, 11 KQVS, 1 HPNWP, 6 CRJZ => 4 DGJPN\n2 DZQJ, 1 BDSB => 2 DGMN\n1 XVXCZ, 4 MQRS => 3 WDFXH\n5 FLHW, 10 JPGK, 1 XZKTG => 4 QPNTQ\n2 LRBN => 9 FZVS\n149 ORE => 8 ZTBFN"

testcase :: String
testcase = "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

testcase1 :: String
testcase1 = "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"

testcase2 :: String
testcase2 = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"

type ChemicalUnit = (Int, String)

data Reactions = Reactions
  { output :: ChemicalUnit
  , inputs :: [ChemicalUnit]
  } deriving (Show)

data Chemicals = Chemicals
  { available :: Map String Int
  , reactions :: Map String Reactions
  , ore :: Int
  } deriving (Show)

int :: Parser Int
int = read <$> many1 digit

chemical :: Parser ChemicalUnit
chemical = do
  unit <- int
  spaces
  name <- many1 upper
  return (unit, name)

chemicalList :: Parser [ChemicalUnit]
chemicalList = chemical `sepBy` string ", "

chemicalLine :: Parser Reactions
chemicalLine = do
  list <- chemicalList
  _ <- string " => "
  chem <- chemical
  return  Reactions { output = chem , inputs = list }

parseInput :: String -> Map String Reactions
parseInput inputStr = 
  case parse parser "" inputStr of
    Right parsed -> foldl toMap Map.empty parsed
    Left _ -> error "Parsing input failed"
  where
    parser = chemicalLine `sepBy` newline
    toMap m chem@(Reactions { output }) = 
      let (_, name) = output
       in Map.insert name chem m

checkAvailable :: String -> Chemicals -> Int
checkAvailable name (Chemicals { available }) =
  Map.findWithDefault 0 name available

subtractAvailable :: String -> Int -> Chemicals -> Chemicals
subtractAvailable name unit chemicals@(Chemicals { available }) = chemicals
  { available = Map.insert name (available' - unit) available }
  where 
    available' = checkAvailable name chemicals

addAvailable :: String -> Int -> Chemicals -> Chemicals
addAvailable name unit chemicals@(Chemicals { available }) = chemicals
  { available = Map.insert name (available' + unit) available }
  where
    available' = checkAvailable name chemicals

multiplier :: Int -> Int -> Int
multiplier required produced
  | r > 0 = n + 1 
  | otherwise = n
  where
    (n, r) = required `divMod` produced

triggerReaction :: String -> Int -> Chemicals -> Chemicals
triggerReaction name required chemicals@(Chemicals { reactions }) =
  let chemical' = reactions Map.! name
      (unit, _) = output chemical' 
      mult = multiplier required unit
      inputs' = (\(u, n) -> (u * mult, n)) <$> inputs chemical'
      newChemicals = foldl count chemicals inputs'
   in addAvailable name (unit * mult) newChemicals
    where
      count :: Chemicals -> ChemicalUnit -> Chemicals
      count chemicals' (unit', "ORE") = chemicals' { ore = (ore chemicals) + unit' }
      count chemicals' (unit', name')
        | (checkAvailable name' chemicals') >= unit' = subtractAvailable name' unit' chemicals'
        | otherwise =
            subtractAvailable name' unit' $ triggerReaction name' required' chemicals'
              where
                required' = unit' - checkAvailable name' chemicals'

trillion :: Int
trillion = 1000000000000

findHigh :: Int -> Chemicals -> Int
findHigh n chemicals = if ore' > trillion then n else findHigh (n * 2) chemicals
  where 
    ore' = ore $ triggerReaction "FUEL" n chemicals

findMaxFuel :: Int -> Int -> Chemicals -> Int
findMaxFuel low high chemicals
  | (high - low) <= 1 = low
  | otherwise = if reaction mid > trillion then findMaxFuel low mid chemicals else findMaxFuel mid high chemicals
  where 
    reaction n = ore $ triggerReaction "FUEL" n chemicals
    mid = (low + high) `div` 2

partOne :: String -> Int
partOne inputStr = ore $ triggerReaction "FUEL" 1 chemicals
  where
    chemicals = Chemicals 
      { available = Map.empty
      , reactions = parseInput inputStr
      , ore = 0
      }

partTwo :: String -> Int
partTwo inputStr = findMaxFuel low high chemicals
  where
    oreForOne = partOne inputStr
    low = trillion `div` oreForOne
    high = findHigh low chemicals
    chemicals = Chemicals 
      { available = Map.empty
      , reactions = parseInput inputStr
      , ore = 0
      }


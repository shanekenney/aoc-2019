module Solve.Day16 where

import Helpers

input :: String
input = "59767332893712499303507927392492799842280949032647447943708128134759829623432979665638627748828769901459920331809324277257783559980682773005090812015194705678044494427656694450683470894204458322512685463108677297931475224644120088044241514984501801055776621459006306355191173838028818541852472766531691447716699929369254367590657434009446852446382913299030985023252085192396763168288943696868044543275244584834495762182333696287306000879305760028716584659188511036134905935090284404044065551054821920696749822628998776535580685208350672371545812292776910208462128008216282210434666822690603370151291219895209312686939242854295497457769408869210686246"

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

parseInput :: String -> [Int]
parseInput inputStr = charToInt <$> inputStr
  where
    charToInt c = parseInt $ c:[]

repeatingPattern :: Int -> [Int]
repeatingPattern n = drop 1 $ cycle $ concatMap (\x -> replicate n x) basePattern

phase :: [Int] -> [Int]
phase signal = calc <$> take (length signal) [1..]
  where 
    calc n = ones $ sum $ map multiplyPair $ zip signal $ repeatingPattern n
    multiplyPair (n, p) = n * p
    ones n = (abs n) `mod` 10

phase2 :: [Int] -> [Int]
phase2 signal = scanr sumMod last' allExceptLast
  where
    last' = last signal
    allExceptLast = init signal
    sumMod x y = (x + y) `mod` 10

partOne :: String -> [Int]
partOne inputStr = take 8 $ nth 100 $ iterate phase input'
  where
    nth = flip (!!)
    input' = parseInput inputStr

partTwo :: String -> [Int]
partTwo inputStr = take 8 $ nth 100 $ iterate phase2 input'
  where
    input' = parseInput $ drop offset $ concat $ replicate 10000 inputStr
    offset = parseInt $ take 7 $ inputStr
    nth = flip (!!)


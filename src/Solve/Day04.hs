module Solve.Day04 where

repeatChar :: String -> Bool
repeatChar n = elem 2 $ length <$> partition n

noDecrease :: String -> Bool
noDecrease n = findDecrease predicate n
  where predicate a b = a <= b

findRepeat :: (Char -> Char -> Char -> Bool) -> String -> Bool
findRepeat _ [] = False
findRepeat predicate (a:b:c:[]) = if predicate a b c then True else False
findRepeat predicate (a:b:c:xs) = if predicate a b c then True else findRepeat predicate (b:c:xs)

findDecrease :: (Char -> Char -> Bool) -> String -> Bool
findDecrease _ [] = True
findDecrease predicate (a:b:[]) = if predicate a b then True else False
findDecrease predicate (a:b:xs) = if predicate a b then findDecrease predicate (b:xs) else False

partition :: String -> [String]
partition input = foldr groupRepeating [] input
  where 
    groupRepeating char [] = [[char]]
    groupRepeating char ((c:cs):[]) = if char == c then (char:c:cs):[] else [[char], c:cs]
    groupRepeating char ((c:cs):xs) = if char == c then (char:c:cs):xs else [char]:(c:cs):xs


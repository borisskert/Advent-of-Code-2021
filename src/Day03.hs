module Day03 where

-- https://adventofcode.com/2021/day/3

mostCommonBits :: [String] -> [Char]
mostCommonBits report = map (`mostCommonBitAt` report) [0 .. maxIndex]
  where
    maxIndex = (+ (-1)) . length . head $ report

mostCommonBitAt :: Int -> [String] -> Char
mostCommonBitAt index report
  | (ones >) . (`div` 2) . length $ report = '1'
  | otherwise = '0'
  where
    ones = length . filter (== '1') . map (!! index) $ report :: Int

flipBits :: String -> String
flipBits [] = []
flipBits (c : cs)
  | c == '1' = '0' : flipBits cs
  | otherwise = '1' : flipBits cs

binToInt :: String -> Int
binToInt [] = 0
binToInt cs
  | last cs == '1' = 1 + 2 * rest
  | otherwise = 2 * rest
  where
    rest = binToInt (init cs)

powerConsumption :: String -> (Int, Int)
powerConsumption input = (epsilon, gamma)
  where
    report = lines input :: [String]
    epsilon = binToInt . mostCommonBits $ report :: Int
    gamma = binToInt . flipBits . mostCommonBits $ report :: Int

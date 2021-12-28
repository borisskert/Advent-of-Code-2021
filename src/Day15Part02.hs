module Day15Part02 where

-- https://adventofcode.com/2021/day/15#part2

import Day15 (Field, lowestRiskLevel, parseInput)

expand :: Field -> Field
expand values = map (rowinger 0) values ++ map (rowinger 1) values ++ map (rowinger 2) values ++ map (rowinger 3) values ++ map (rowinger 4) values
  where
    rowinger :: Int -> [Int] -> [Int]
    rowinger i x = map (offset i) x ++ map (offset (i + 1)) x ++ map (offset (i + 2)) x ++ map (offset (i + 3)) x ++ map (offset (i + 4)) x

    offset :: Int -> Int -> Int
    offset i x
      | x + i > 9 = x + i + 1 - 10
      | otherwise = x + i

lowestTotalExtended :: String -> Int
lowestTotalExtended = lowestRiskLevel . expand . parseInput

module Day18Part02 where

-- https://adventofcode.com/2021/day/18#part2

import Day18 (Snailfish, add, magnitude, parseInput)

largestMagnitude :: String -> Int
largestMagnitude input = maximum . map largestOf $ combinations
  where
    snailfishs = parseInput input :: [Snailfish]
    combinations = (\snailfishs -> [(x, y) | x <- snailfishs, y <- snailfishs, x /= y]) snailfishs :: [(Snailfish, Snailfish)]

largestOf :: (Snailfish, Snailfish) -> Int
largestOf (s1, s2) = maximum [s1AddS2, s2AddS1]
  where
    s1AddS2 = magnitude $ s1 `add` s2 :: Int
    s2AddS1 = magnitude $ s2 `add` s1 :: Int

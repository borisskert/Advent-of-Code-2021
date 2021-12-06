module Day06Part02 where

import Day06 (parseInput)

-- https://adventofcode.com/2021/day/6#part2

laternfish :: Int -> Int -> Int
laternfish days timer
  | days <= timer = 1
  | otherwise =
    (+ 1)
      . sum
      . map (`laternfish` 8)
      $ birthdays
  where
    initialTimer = 6
    birthdays =
      takeWhile (>= 0)
        . iterate (+ (- initialTimer - 1))
        . (+ (- timer))
        $ (days - 1)

simulateLanternfishExtended :: Int -> String -> Int
simulateLanternfishExtended days = sum . map (laternfish days) . parseInput

module Day07 where

-- https://adventofcode.com/2021/day/7

import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

cheapestPossibleOutcome :: String -> Int
cheapestPossibleOutcome input =
  minimum
    . map (sum . (\x -> map (\y -> abs (x - y)) positions))
    $ positions
  where
    positions = parseInput input

module Day07Part02 where

-- https://adventofcode.com/2021/day/7#part2

import Day07 (parseInput)

cheapestPossibleOutcomeExtended :: String -> Int
cheapestPossibleOutcomeExtended input =
  minimum
    . map (sum . (\x -> map (fuelCosts . (abs . (+ (-x)))) positions))
    $ possiblePositions
  where
    positions = parseInput input
    possiblePositions = [minimum positions .. maximum positions]

fuelCosts :: Int -> Int
fuelCosts x = sum [1..x]

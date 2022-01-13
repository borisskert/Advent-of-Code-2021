module Day22Part02 where

-- https://adventofcode.com/2021/day/22#part2

import Day22 (RebootSteps, howManyAreOn, parseInput)

howManyCubesAreOnExtended :: String -> Integer
howManyCubesAreOnExtended input = howManyAreOn steps
  where
    steps = parseInput input :: RebootSteps

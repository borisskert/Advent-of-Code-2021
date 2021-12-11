module Day11Part02 where

-- https://adventofcode.com/2021/day/11#part2

import Day11 (Octopus (Octopus), isFlashing, octopuses, parseInput, resetEnergyIfFlashing, step)

flashSimultaneously :: String -> Int
flashSimultaneously input =
  fst
    . head
    . dropWhile (not . areAllFlashing . snd)
    . iterate steps
    $ (0, all)
  where
    all = octopuses . parseInput $ input :: [Octopus]

    steps :: (Int, [Octopus]) -> (Int, [Octopus])
    steps (i, os) = (i + 1, step . map resetEnergyIfFlashing $ os)

areAllFlashing :: [Octopus] -> Bool
areAllFlashing = all isFlashing

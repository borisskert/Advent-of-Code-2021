module Day06 where

import Data.List.Split (splitOn)

-- https://adventofcode.com/2021/day/6

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

lanternfish :: Int -> [Int] -> [Int]
lanternfish 0 fish = fish
lanternfish days fish = lanternfish (days - 1) . (++ producedFish) . map internalTimer $ fish
  where
    producedFish = map (const 8) . filter (== 0) $ fish

    internalTimer :: Int -> Int
    internalTimer 0 = 6
    internalTimer x = x - 1

simulateLanternfish :: Int -> String -> Int
simulateLanternfish days = length . lanternfish days . parseInput

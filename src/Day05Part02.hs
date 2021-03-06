{-# LANGUAGE TupleSections #-}

module Day05Part02 where

import Data.List (nub, sort, group)
import Day05 (parseInput)

-- https://adventofcode.com/2021/day/5#part2

extract :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
extract ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) [minimum [y1, y2] .. maximum [y1, y2]]
  | y1 == y2 = map (,y1) [minimum [x1, x2] .. maximum [x1, x2]]
  | abs (x1 - x2) == abs (y1 - y2) = extractDiagonal ((x1, y1), (x2, y2))
  | otherwise = []
  where
    extractDiagonal :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
    extractDiagonal ((x1, y1), (x2, y2))
      | x1 < x2 && y1 < y2 = zip [x1 .. x2] [y1 .. y2]
      | x1 > x2 && y1 > y2 = zip [x2 .. x1] [y2 .. y1]
      | x1 < x2 && y1 > y2 = zip [x1 .. x2] [y1, (y1 - 1) .. y2]
      | x1 > x2 && y1 < y2 = zip [x2 .. x1] [y2, (y2 - 1) .. y1]

overlapDiagonal :: String -> Int
overlapDiagonal = length . filter (> 1) . count . concatMap extract . parseInput
  where
    count :: [(Int, Int)] -> [Int]
    count = map length . group . sort

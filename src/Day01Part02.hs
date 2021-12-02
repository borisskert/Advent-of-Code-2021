module Day01Part02 where

-- https://adventofcode.com/2021/day/1#part2

import Data.List.Split (divvy)

countIncreasedSums :: String -> Int
countIncreasedSums measurement =
  length
    . filter snd
    . scanl (\x y -> (y, y > fst x)) (head sums, False)
    . tail $ sums
  where
    depths = map read . lines $ measurement :: [Int]
    sums = map sum . divvy 3 1 $ depths

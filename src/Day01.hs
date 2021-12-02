module Day01 where

-- https://adventofcode.com/2021/day/1

sonarSweep :: String -> Int
sonarSweep measurement =
  length
    . filter snd
    . scanl (\x y -> (y, y > fst x)) (head depths, False)
    . tail $ depths
  where
    depths = map read . lines $ measurement :: [Int]

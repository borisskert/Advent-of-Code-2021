module Day02Part02 where

-- https://adventofcode.com/2021/day/2#part2

import Data.Bifunctor (Bifunctor (second))
import Data.List.Split (splitOn)

submarineDriveAim :: String -> (Int, Int)
submarineDriveAim = fst . foldl movement ((0, 0), 0) . map parse . lines

parse :: String -> (String, Int)
parse = second read . (\split -> (head split, last split)) . splitOn " "

movement :: ((Int, Int), Int) -> (String, Int) -> ((Int, Int), Int)
movement ((horizontal, depth), aim) ("forward", x) = ((horizontal + x, depth + aim * x), aim)
movement (position, aim) ("down", x) = (position, aim + x)
movement (position, aim) ("up", x) = (position, aim - x)

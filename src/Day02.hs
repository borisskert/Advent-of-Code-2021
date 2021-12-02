module Day02 where

-- https://adventofcode.com/2021/day/2

import Data.Bifunctor (Bifunctor (second))
import Data.List.Split (splitOn)

submarineDrive :: String -> (Int, Int)
submarineDrive = foldl add (0, 0) . map (movement . parse) . lines

parse :: String -> (String, Int)
parse = second read . (\split -> (head split, last split)) . splitOn " "

movement :: (String, Int) -> (Int, Int)
movement ("forward", x) = (x, 0)
movement ("down", x) = (0, x)
movement ("up", x) = (0, - x)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (h1, d1) (h2, d2) = (h1 + h2, d1 + d2)

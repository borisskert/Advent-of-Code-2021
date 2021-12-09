module Day09 where

import Data.Char (digitToInt)

-- https://adventofcode.com/2021/day/9

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show)

parseInput :: String -> [(Point, Int)]
parseInput = mapFromInts 0 . map (map digitToInt) . lines
  where
    mapFromInts :: Int -> [[Int]] -> [(Point, Int)]
    mapFromInts _ [] = []
    mapFromInts i (line : lines) = point ++ mapFromInts (i + 1) lines
      where
        size = length line :: Int
        point = map ((\p -> (p, line !! y p)) . (\y -> (Point {x = i, y = y}))) [0 .. (size - 1)]

lowPoints :: [(Point, Int)] -> [(Point, Int)]
lowPoints points = filter isLowPoint points
  where
    isLowPoint :: (Point, Int) -> Bool
    isLowPoint (p, i) =
      all ((> i) . snd)
        . filter (isNeighbor . fst)
        $ points
      where
        isNeighbor :: Point -> Bool
        isNeighbor n
          | x n == x p = abs (y n - y p) == 1
          | y n == y p = abs (x n - x p) == 1
          | otherwise = False

riskLevel :: String -> Int
riskLevel = sum . map ((+ 1) . snd) . lowPoints . parseInput

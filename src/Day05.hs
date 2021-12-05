{-# LANGUAGE TupleSections #-}

module Day05 where

-- https://adventofcode.com/2021/day/5

import Data.List (nub)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

parseInput :: String -> [((Int, Int), (Int, Int))]
parseInput = map ((\x -> (head x, last x)) . map toTuple . splitOn " -> ") . lines
  where
    toTuple :: String -> (Int, Int)
    toTuple = (\x -> (read . head $ x, read . last $ x)) . splitOn ","

extract :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
extract ((x1, y1), (x2, y2))
  | x1 == x2 = extractY (x1, y1) (x2, y2)
  | y1 == y2 = extractX (x1, y1) (x2, y2)
  | otherwise = []
  where
    extractX :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    extractX (x1, y1) (x2, y2)
      | x1 < x2 = map (,y1) [x1 .. x2]
      | x1 > x2 = map (,y1) [x1, (x1 - 1) .. x2]
      | otherwise = [(x1, y1)]

    extractY :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    extractY (x1, y1) (x2, y2)
      | y1 < y2 = map (x1,) [y1 .. y2]
      | y1 > y2 = map (x1,) [y1, (y1 - 1) .. y2]
      | otherwise = [(x1, y1)]

overlap :: String -> Int
overlap = length . filter (> 1) . map snd . count . concatMap extract . parseInput
  where
    count :: [(Int, Int)] -> [((Int, Int), Int)]
    count xs = map withCount . nub $ xs
      where
        withCount x = (x,) . length . filter (== x) $ xs
        withCount :: (Int, Int) -> ((Int, Int), Int)

module Day19Part02 where

-- https://adventofcode.com/2021/day/19#part2

import Day19 (Scan, parseInput, toScans, absoluteBeaconPositions, Vector, Vectors, beaconsFrom, relativeScannerPositions)
import Data.List (nub)
import Data.Maybe (isJust, fromJust, isNothing)

scannerPosition :: Scan -> Vector
scannerPosition (position, _, _) = position

absoluteScannerPositions :: [Scan] -> Vectors
absoluteScannerPositions (scan0 : scans) = nub . (scanner0Position :) . search $ ([scan0], scans)
  where
    scanner0Position = scannerPosition scan0 :: Vector

    search :: ([Scan], [Scan]) -> Vectors
    search (_, []) = []
    search (scan0 : otherScans, open) = nub . (map scannerPosition found ++) $ search (otherScans ++ found, newOpen)
      where
        scannerPositions = map (\o -> (o, relativeScannerPositions scan0 o)) open :: [(Scan, Maybe Scan)]
        found = map (fromJust . snd) . filter (isJust . snd) $ scannerPositions :: [Scan]
        newOpen = map fst . filter (isNothing . snd) $ scannerPositions :: [Scan]

manhattanDistance :: Vector -> Vector -> Int
manhattanDistance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

largestManhattanDistance :: String -> Int
largestManhattanDistance input = maximum . map (uncurry manhattanDistance) $ combinations
  where
    scans = toScans . parseInput $ input :: [Scan]
    scannerPositions = absoluteScannerPositions scans :: Vectors
    combinations = (\xs -> [(x,y) | x <- xs, y <- xs, x /= y]) $ scannerPositions :: [(Vector, Vector)]

module Day06Part02 where

import Data.List (nub)
import Data.List.Split (splitOn)

-- https://adventofcode.com/2021/day/6#part2

parseInput :: String -> [(Int, Integer)]
parseInput input = map (\x -> (\xs -> (x, toInteger . length $ xs)) . filter (== x) $ values) $ unique
  where
    values = map read . splitOn "," $ input :: [Int]
    unique = nub values :: [Int]

laternfish :: Integer -> [(Int, Integer)] -> [(Int, Integer)]
laternfish 0 fish = fish
laternfish days fish = laternfish (days -1) (merge nonParents children)
  where
    nextFish = map (\(timer, count) -> (timer -1, count)) fish :: [(Int, Integer)]
    nonParents = filter (\(timer, _) -> timer >= 0) nextFish :: [(Int, Integer)]
    parents = filter (\(timer, _) -> timer < 0) nextFish :: [(Int, Integer)]
    children = concatMap (\(timer, count) -> [(6, count), (8, count)]) parents :: [(Int, Integer)]

    merge :: [(Int, Integer)] -> [(Int, Integer)] -> [(Int, Integer)]
    merge ax bx = old ++ merged ++ new
      where
        old = filter (\(aTimer, _) -> not . any (\(bTimer, _) -> aTimer == bTimer) $ bx) $ ax
        merged = concatMap (\(bTimer, bCount) -> map (\(_, aCount) -> (bTimer, bCount + aCount)) . filter (\(aTimer, _) -> aTimer == bTimer) $ ax) bx
        new = filter (\(aTimer, _) -> not . any (\(bTimer, _) -> aTimer == bTimer) $ ax) $ bx

simulateLanternfishExtended :: Integer -> String -> Integer
simulateLanternfishExtended days = sum . map snd . laternfish days . parseInput

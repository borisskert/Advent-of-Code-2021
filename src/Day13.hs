module Day13 where

-- https://adventofcode.com/2021/day/13

import Data.List (nub)
import Data.List.Split (splitOn)

parseInput :: String -> ([(Int, Int)], [(Char, Int)])
parseInput input = (positions, instructions)
  where
    positionLines = takeWhile (not . null) . lines $ input :: [String]
    foldInstructionLines = dropWhile null . dropWhile (not . null) . lines $ input :: [String]

    positions = map ((\xs -> (head xs, last xs)) . map read . splitOn ",") positionLines :: [(Int, Int)]
    instructions = map parseInstruction foldInstructionLines :: [(Char, Int)]

    parseInstruction :: String -> (Char, Int)
    parseInstruction = (\xs -> (head . head $ xs, read . last $ xs)) . splitOn "=" . last . words

foldBy :: (Int, Int) -> (Char, Int) -> Maybe (Int, Int)
foldBy (x, y) ('x', foldX)
  | x == foldX = Nothing
  | x < foldX = Just (x, y)
  | otherwise = Just (2 * foldX - x, y)
foldBy (x, y) ('y', foldY)
  | y == foldY = Nothing
  | y < foldY = Just (x, y)
  | otherwise = Just (x, 2 * foldY - y)

howManyDots :: String -> Int
howManyDots input = length . nub . map (`foldBy` instruction) $ positions
  where
    (positions, instructions) = parseInput input
    instruction = head instructions :: (Char, Int)

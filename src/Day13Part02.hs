module Day13Part02 where

-- https://adventofcode.com/2021/day/13#part2

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Day13 (foldBy, parseInput)

foldByAll :: (Int, Int) -> [(Char, Int)] -> Maybe (Int, Int)
foldByAll position = foldl go (Just position)
  where
    go :: Maybe (Int, Int) -> (Char, Int) -> Maybe (Int, Int)
    go (Just position) instruction = foldBy position instruction
    go Nothing _ = Nothing

dots :: String -> [(Int, Int)]
dots input = nub . mapMaybe (`foldByAll` instructions) $ positions
  where
    (positions, instructions) = parseInput input
    instruction = head instructions :: (Char, Int)

toGrid :: [(Int, Int)] -> [[Bool]]
toGrid dots = map toRow $ [0 .. height]
  where
    width = maximum . map fst $ dots :: Int
    height = maximum . map snd $ dots :: Int

    toRow :: Int -> [Bool]
    toRow y = map (value . (\x -> (x, y))) $ [0 .. width]

    value :: (Int, Int) -> Bool
    value (x, y) = not . null $ found
      where
        found = filter (\(x1, y1) -> x1 == x && y1 == y) $ dots

printDots :: String -> String
printDots = unlines . map printRow . toGrid . dots
  where
    printRow :: [Bool] -> String
    printRow = map (\b -> if b then '#' else '.')

howManyDots :: String -> Int
howManyDots = length . dots

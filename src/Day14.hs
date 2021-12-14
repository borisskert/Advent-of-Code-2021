module Day14 where

-- https://adventofcode.com/2021/day/14

import Data.List (nub)
import Data.List.Split (divvy, splitOn)
import Data.Map (Map, fromList, lookup, member)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

parseInput :: String -> (String, [(String, String)])
parseInput input = (template, pairInsertions)
  where
    inputLines = lines input

    template = head . takeWhile (not . null) $ inputLines :: String
    pairInsertions = map toPairInsertion . dropWhile null . dropWhile (not . null) $ inputLines

    toPairInsertion :: String -> (String, String)
    toPairInsertion = (\(left : right : _) -> (left, right)) . splitOn " -> "

replacementRules :: [(String, String)] -> Map String String
replacementRules = fromList

step :: String -> Map String String -> String
step [] _ = []
step template replacementRules = (++ [last template]) . concatMap replace $ pairs
  where
    keyLength = 2 :: Int
    pairs = divvy keyLength 1 template :: [String]

    replace :: String -> String
    replace search
      | member search replacementRules = take 1 search ++ replacement
      | otherwise = take 1 search
      where
        replacement = fromJust . lookup search $ replacementRules

steps :: Int -> String -> Map String String -> String
steps 0 template _ = template
steps i template rules = steps (i - 1) (step template rules) rules

polymerElements :: String -> (Int, Int)
polymerElements input = (maximum counts, minimum counts)
  where
    (template, pairInsertions) = parseInput input
    rules = replacementRules pairInsertions :: Map String String
    result = steps 10 template rules :: String
    elements = nub result :: String

    countedElements = map (\e -> (e, length . filter (== e) $ result)) elements :: [(Char, Int)]

    counts = map snd countedElements :: [Int]

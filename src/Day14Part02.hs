module Day14Part02 where

-- https://adventofcode.com/2021/day/14#part2

import Data.List (group, groupBy, nub, sort, sortOn)
import Data.List.Split (divvy)
import Data.Map (Map, delete, fromList, insert, lookup, member, toList)
import Data.Maybe (fromJust)
import Day14 (parseInput)
import Prelude hiding (lookup)

data Pair = Pair (Char, Char) deriving (Eq, Show, Ord)

data Rules = Rules (Map Pair Char) deriving (Eq, Show)

makeRules :: [([Char], [Char])] -> Rules
makeRules = Rules . fromList . map (\(left : right : _, [r]) -> (Pair (left, right), r))

replacement :: Pair -> Rules -> Char
replacement pair (Rules rules) = fromJust . lookup pair $ rules

disassemble :: String -> Map Pair Integer
disassemble =
  fromList
    . map (\xs -> (toPair . head $ xs, toInteger . length $ xs))
    . group
    . sort
    . divvy 2 1
  where
    toPair :: String -> Pair
    toPair (a : b : _) = Pair (a, b)

count :: Map Pair Integer -> Map Char Integer
count =
  fromList
    . map fromGroup
    . groupBy sameGroup
    . sort
    . map toCount
    . toList
  where
    fromGroup :: [(Char, Integer)] -> (Char, Integer)
    fromGroup xs = (c, i)
      where
        c = fst . head $ xs :: Char
        i = sum . map snd $ xs

    toCount :: (Pair, Integer) -> (Char, Integer)
    toCount pair = (a, i)
      where
        (Pair (a, _), i) = pair

    sameGroup :: (Char, Integer) -> (Char, Integer) -> Bool
    sameGroup (a, _) (b, _) = a == b

addOne :: Map Char Integer -> Char -> Map Char Integer
addOne counters c
  | member c counters = insert c (value + 1) counters
  | otherwise = insert c 1 counters
  where
    value = fromJust . lookup c $ counters

addPair :: Map Pair Integer -> (Pair, Integer) -> Map Pair Integer
addPair pairs (pair, i)
  | member pair pairs = insert pair (value + i) pairs
  | otherwise = insert pair i pairs
  where
    value = fromJust . lookup pair $ pairs

removePair :: Map Pair Integer -> (Pair, Integer) -> Map Pair Integer
removePair pairs (pair, i)
  | newValue == 0 = delete pair pairs
  | otherwise = insert pair (value - i) pairs
  where
    value = fromJust . lookup pair $ pairs
    newValue = value - i

perform :: Rules -> String -> Int -> [(Char, Integer)]
perform rules text i = toList . run pairs $ i
  where
    pairs = disassemble text

    run :: Map Pair Integer -> Int -> Map Char Integer
    run pairs i
      | i == 0 = (`addOne` last text) . count $ pairs
      | otherwise = run nextStep (i - 1)
      where
        nextStep = foldl reduce pairs . toList $ pairs

        reduce :: Map Pair Integer -> (Pair, Integer) -> Map Pair Integer
        reduce countedPairs (pair, i) =
          (`addPair` (pairB, i))
            . (`addPair` (pairA, i))
            . (`removePair` (pair, i))
            $ countedPairs
          where
            (Pair (a, b)) = pair
            c = replacement pair rules
            pairA = Pair (a, c)
            pairB = Pair (c, b)

polymerElementsExtended :: String -> Int -> (Integer, Integer)
polymerElementsExtended input steps = (maximum . map snd $ counts, minimum . map snd $ counts)
  where
    (template, pairInsertions) = parseInput input
    rules = makeRules pairInsertions :: Rules
    counts = perform rules template steps :: [(Char, Integer)]

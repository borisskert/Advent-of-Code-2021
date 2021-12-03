module Day03Part02 where

import Day03 (mostCommonBits)
import Debug.Trace (traceShow)
import Data.Maybe (fromMaybe, isNothing)

-- https://adventofcode.com/2021/day/3#part2

mostCommonBitAt :: Int -> [String] -> Maybe Char
mostCommonBitAt _ [] = error "It's empty!"
mostCommonBitAt index report
  | ones == zeros = Nothing
  | ones > zeros = Just '1'
  | otherwise = Just '0'
  where
    ones = length . filter (== '1') . map (!! index) $ report :: Int
    zeros = length . filter (== '0') . map (!! index) $ report :: Int

leastCommonBitAt :: Int -> [String] -> Maybe Char
leastCommonBitAt _ [] = error "It's empty!"
leastCommonBitAt index report
  | isNothing mostCommonBit = Nothing
  | mostCommonBit == Just '1' = Just '0'
  | otherwise = Just '1'
  where
    mostCommonBit = mostCommonBitAt index report :: Maybe Char

binToInt :: String -> Int
binToInt [] = 0
binToInt cs
  | last cs == '1' = 1 + 2 * rest
  | otherwise = 2 * rest
  where
    rest = binToInt (init cs)

oxygenGeneratorRating :: [String] -> String
oxygenGeneratorRating [] = []
oxygenGeneratorRating report = mostCommonBit : oxygenGeneratorRating nextReport
  where
    mostCommonBit = fromMaybe '1' . mostCommonBitAt 0 $ report :: Char
    nextReport = filter (not . null) . map tail . filter (\x -> head x == mostCommonBit) $ report :: [String]

co2ScrubberRating :: [String] -> String
co2ScrubberRating [] = []
co2ScrubberRating [s] = s
co2ScrubberRating report = leastCommonBit : co2ScrubberRating nextReport
  where
    leastCommonBit = fromMaybe '0' . leastCommonBitAt 0 $ report :: Char
    nextReport = filter (not . null) . map tail . filter (\x -> head x == leastCommonBit) $ report :: [String]

lifeSupportRating :: String -> (Int, Int)
lifeSupportRating input = (oxygen, co2)
  where
    report = lines input :: [String]
    oxygen = binToInt . oxygenGeneratorRating $ report :: Int
    co2 = binToInt . co2ScrubberRating $ report :: Int

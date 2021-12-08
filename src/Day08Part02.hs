module Day08Part02 where

-- https://adventofcode.com/2021/day/8#part2

import Day08 (parseInput, toDigits)

summarizeDigits :: String -> Int
summarizeDigits = sum . map digitsToInt . toDigits

digitsToInt :: [Int] -> Int
digitsToInt [x] = x
digitsToInt xs = last xs + 10 * digitsToInt (init xs)
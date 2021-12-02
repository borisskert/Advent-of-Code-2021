module Main where

import Day01 (countIncreases)
import Day01Part02 (countIncreasedSums)
import System.IO

main :: IO ()
main = do
  day01
  day01Part2
  where
    day01Part2 :: IO ()
    day01Part2 = do
      day01Input <- readFileContents "app/day01_input.txt"
      let result = countIncreasedSums day01Input
      putStr "Day 01/Part 02: "
      print result

    day01 :: IO ()
    day01 = do
      day01Input <- readFileContents "app/day01_input.txt"
      let result = countIncreases day01Input
      putStr "Day 01: "
      print result

readFileContents :: String -> IO String
readFileContents filename = do
  fileHandle <- openFile filename ReadMode
  hGetContents fileHandle

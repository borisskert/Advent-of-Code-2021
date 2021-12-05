module Main where

import Day01 (countIncreases)
import Day01Part02 (countIncreasedSums)
import Day02 (submarineDrive)
import Day02Part02 (submarineDriveAim)
import Day03 (powerConsumption)
import Day03Part02 (lifeSupportRating)
import Day04 (playSubmarineBingo)
import Day04Part02 (letSquidWin)
import Day05 (overlap)
import Day05Part02 (overlapDiagonal)
import System.IO

main :: IO ()
main = do
  day05Part02
  day05
  day04Part02
  day04
  day03Part02
  day03
  day02Part02
  day02
  day01Part2
  day01
  where
    day05Part02 :: IO ()
    day05Part02 = do
      input <- readFileContents "app/day05_input.txt"
      let result = overlapDiagonal input
      putStr "Day 05/Part 02: -> "
      print result

    day05 :: IO ()
    day05 = do
      input <- readFileContents "app/day05_input.txt"
      let result = overlap input
      putStr "Day 05: -> "
      print result

    day04Part02 :: IO ()
    day04Part02 = do
      input <- readFileContents "app/day04_input.txt"
      let result = letSquidWin input
      putStr "Day 04/Part 02: "
      print result
      putStr " -> "
      print (uncurry (*) result)

    day04 :: IO ()
    day04 = do
      input <- readFileContents "app/day04_input.txt"
      let result = playSubmarineBingo input
      putStr "Day 04: "
      print result
      putStr " -> "
      print (uncurry (*) result)

    day03Part02 :: IO ()
    day03Part02 = do
      input <- readFileContents "app/day03_input.txt"
      let result = lifeSupportRating input
      putStr "Day 03/Part 02: "
      print result
      putStr " -> "
      print (uncurry (*) result)

    day03 :: IO ()
    day03 = do
      input <- readFileContents "app/day03_input.txt"
      let result = powerConsumption input
      putStr "Day 03: "
      print result
      putStr " -> "
      print (uncurry (*) result)

    day02Part02 :: IO ()
    day02Part02 = do
      input <- readFileContents "app/day02_input.txt"
      let result = submarineDriveAim input
      putStr "Day 02/Part 02: "
      print result
      putStr " -> "
      print (uncurry (*) result)

    day02 :: IO ()
    day02 = do
      input <- readFileContents "app/day02_input.txt"
      let result = submarineDrive input
      putStr "Day 02: "
      print result
      putStr " -> "
      print (uncurry (*) result)

    day01Part2 :: IO ()
    day01Part2 = do
      input <- readFileContents "app/day01_input.txt"
      let result = countIncreasedSums input
      putStr "Day 01/Part 02: "
      print result

    day01 :: IO ()
    day01 = do
      input <- readFileContents "app/day01_input.txt"
      let result = countIncreases input
      putStr "Day 01: "
      print result

readFileContents :: String -> IO String
readFileContents filename = do
  fileHandle <- openFile filename ReadMode
  hGetContents fileHandle

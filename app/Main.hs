module Main where

import Day01 (countIncreases)
import Day01Part02 (countIncreasedSums)
import Day02 (submarineDrive)
import Day02Part02 (submarineDriveAim)
import System.IO

main :: IO ()
main = do
  day01
  day01Part2
  day02
  day02Part02
  where
    day02Part02 :: IO ()
    day02Part02 = do
      input <- readFileContents "app/day02_input.txt"
      let result = submarineDriveAim input
      putStr "Day 02/Part02: "
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

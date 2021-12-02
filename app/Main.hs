module Main where

import Day01 (sonarSweep)
import System.IO

main :: IO ()
main = do
  day01
  where
    day01 :: IO ()
    day01 = do
      day01Input <- readFileContents "app/day01_input.txt"
      let result = sonarSweep day01Input
      putStr "Day 01: "
      print result

readFileContents :: String -> IO String
readFileContents filename = do
  fileHandle <- openFile filename ReadMode
  hGetContents fileHandle

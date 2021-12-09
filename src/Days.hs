module Days where

import qualified Data.Map as Map (Map, fromList, lookup, toList)
import Data.Maybe (mapMaybe)
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
import Day06 (simulateLanternfish)
import Day06Part02 (simulateLanternfishExtended)
import Day07 (cheapestPossibleOutcome)
import Day07Part02 (cheapestPossibleOutcomeExtended)
import Day08 (digitsAppear)
import Day08Part02 (summarizeDigits)
import Day09 (riskLevel)
import Day09Part02 (largestBasins)
import System.IO

data Day = Day {run :: IO (), isDefault :: Bool, name :: String, friendlyName :: String}

days :: [Day]
days =
  [ ( Day
        { name = "day09part02",
          friendlyName = "Day 09/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day09_input.txt"
            let result = largestBasins input
            print result
            putStr " --> "
            print (product result)
        }
    ),( Day
        { name = "day09",
          friendlyName = "Day 09",
          isDefault = True,
          run = do
            input <- readFileContents "app/day09_input.txt"
            let result = riskLevel input
            print result
        }
    ),
    ( Day
        { name = "day08part02",
          friendlyName = "Day 08/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day08_input.txt"
            let result = summarizeDigits input
            print result
        }
    ),
    ( Day
        { name = "day08",
          friendlyName = "Day 08",
          isDefault = True,
          run = do
            input <- readFileContents "app/day08_input.txt"
            let result = digitsAppear [1, 4, 7, 8] input
            print result
        }
    ),
    ( Day
        { name = "day07part02",
          friendlyName = "Day 07/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day07_input.txt"
            let result = cheapestPossibleOutcomeExtended input
            print result
        }
    ),
    ( Day
        { name = "day07",
          friendlyName = "Day 07",
          isDefault = True,
          run = do
            input <- readFileContents "app/day07_input.txt"
            let result = cheapestPossibleOutcome input
            print result
        }
    ),
    ( Day
        { name = "day06part02",
          friendlyName = "Day 06/Part02",
          isDefault = False,
          run = do
            input <- readFileContents "app/day06_input.txt"
            let result = simulateLanternfishExtended 256 input
            print result
        }
    ),
    ( Day
        { name = "day06",
          friendlyName = "Day 06",
          isDefault = False,
          run = do
            input <- readFileContents "app/day06_input.txt"
            let result = simulateLanternfish 80 input
            print result
        }
    ),
    ( Day
        { name = "day05part02",
          friendlyName = "Day 05/Part 02",
          isDefault = False,
          run = do
            input <- readFileContents "app/day05_input.txt"
            let result = overlapDiagonal input
            print result
        }
    ),
    ( Day
        { name = "day05",
          friendlyName = "Day 05",
          isDefault = False,
          run = do
            input <- readFileContents "app/day05_input.txt"
            let result = overlap input
            print result
        }
    ),
    ( Day
        { name = "day04part02",
          friendlyName = "Day 04/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day04_input.txt"
            let result = letSquidWin input
            print result
            putStr " --> "
            print (uncurry (*) result)
        }
    ),
    ( Day
        { name = "day04",
          friendlyName = "Day 04",
          isDefault = True,
          run = do
            input <- readFileContents "app/day04_input.txt"
            let result = playSubmarineBingo input
            print result
            putStr " --> "
            print (uncurry (*) result)
        }
    ),
    ( Day
        { name = "day03part02",
          friendlyName = "Day 03/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day03_input.txt"
            let result = lifeSupportRating input
            print result
            putStr " --> "
            print (uncurry (*) result)
        }
    ),
    ( Day
        { name = "day03",
          friendlyName = "Day 03",
          isDefault = True,
          run = do
            input <- readFileContents "app/day03_input.txt"
            let result = powerConsumption input
            print result
            putStr " --> "
            print (uncurry (*) result)
        }
    ),
    ( Day
        { name = "day02part02",
          friendlyName = "Day 02/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day02_input.txt"
            let result = submarineDriveAim input
            print result
            putStr " --> "
            print (uncurry (*) result)
        }
    ),
    ( Day
        { name = "day02",
          friendlyName = "Day 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day02_input.txt"
            let result = submarineDrive input
            print result
            putStr " --> "
            print (uncurry (*) result)
        }
    ),
    ( Day
        { name = "day01part02",
          friendlyName = "Day 01/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day01_input.txt"
            let result = countIncreasedSums input
            print result
        }
    ),
    ( Day
        { name = "day01",
          friendlyName = "Day 01",
          isDefault = True,
          run = do
            input <- readFileContents "app/day01_input.txt"
            let result = countIncreases input
            print result
        }
    )
  ]

runDays :: [String] -> IO ()
runDays args
  | null args = runDefault
  | args == ["all"] = runAll
  | otherwise = mapM_ runOrNotFound args
  where
    runOrNotFound :: String -> IO ()
    runOrNotFound input
      | null foundDays = do
        putStrLn (input ++ " not found.")
      | otherwise = runDay . head $ foundDays
      where
        foundDays = filter ((== input) . name) days

runAll :: IO ()
runAll = mapM_ runDay days

runDefault :: IO ()
runDefault = mapM_ runOrOmit days
  where
    runOrOmit :: Day -> IO ()
    runOrOmit day
      | isDefault day = runDay day
      | otherwise = omitDay day

runDay :: Day -> IO ()
runDay day = do
  putStrLn ("Running " ++ friendlyName day ++ " (" ++ name day ++ ") ...")
  putStr " --> "
  run day
  putStrLn "Finished."

omitDay :: Day -> IO ()
omitDay day = do
  putStrLn (friendlyName day ++ " (" ++ name day ++ ") omitted.")

readFileContents :: String -> IO String
readFileContents filename = do
  fileHandle <- openFile filename ReadMode
  hGetContents fileHandle

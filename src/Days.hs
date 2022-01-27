module Days where

import Control.StopWatch (stopWatch)
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
import Day10 (syntaxErrorScore)
import Day10Part02 (middleScore)
import Day11 (totalFlashes)
import Day11Part02 (flashSimultaneously)
import Day12 (howManyPaths)
import Day12Part02 (howManyPathsV2)
import Day13 (howManyDots)
import Day13Part02 (printDots)
import Day14 (polymerElements)
import Day14Part02 (polymerElementsExtended)
import Day15 (lowestTotal)
import Day15Part02 (lowestTotalExtended)
import Day16 (versionSum)
import Day16Part02 (evaluate)
import Day17 (bestShotMaximumHeight)
import Day17Part02 (howManyDifferentVelocities)
import Day18 (finalSnailfishSum)
import Day18Part02 (largestMagnitude)
import Day19 (howManyBeacons)
import Day19Part02 (largestManhattanDistance)
import Day20 (howManyPixelsAreLit)
import Day20Part02 (howManyPixelsAreLitExtended)
import Day21 (losingSituation)
import Day21Part02 (howManyUniverses)
import Day22 (howManyCubesAreOn)
import Day22Part02 (howManyCubesAreOnExtended)
import Day23 (leastEnergyRequired)
import Day23Part02 (leastEnergyRequiredExtended)
import System.Clock (TimeSpec (TimeSpec), nsec, sec)
import System.IO
import Text.Printf (printf)

data Day = Day {run :: IO (), isDefault :: Bool, name :: String, friendlyName :: String}

days :: [Day]
days =
  [ ( Day
        { name = "day23part02",
          friendlyName = "Day 23/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day23_input.txt"
            let result = leastEnergyRequiredExtended input
            print result
        }
    ),
    ( Day
        { name = "day23",
          friendlyName = "Day 23",
          isDefault = True,
          run = do
            input <- readFileContents "app/day23_input.txt"
            let result = leastEnergyRequired input
            print result
        }
    ),
    ( Day
        { name = "day22part02",
          friendlyName = "Day 22/Part02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day22_input.txt"
            let result = howManyCubesAreOnExtended input
            print result
        }
    ),
    ( Day
        { name = "day22",
          friendlyName = "Day 22",
          isDefault = True,
          run = do
            input <- readFileContents "app/day22_input.txt"
            let result = howManyCubesAreOn input
            print result
        }
    ),
    ( Day
        { name = "day21part02",
          friendlyName = "Day 21/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day21_input.txt"
            let result = howManyUniverses input
            print result
        }
    ),
    ( Day
        { name = "day21",
          friendlyName = "Day 21",
          isDefault = True,
          run = do
            input <- readFileContents "app/day21_input.txt"
            let result = losingSituation input
            print result
            putStr " --> "
            print (uncurry (*) result)
        }
    ),
    ( Day
        { name = "day20part02",
          friendlyName = "Day 20/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day20_input.txt"
            let result = howManyPixelsAreLitExtended input 50
            print result
        }
    ),
    ( Day
        { name = "day20",
          friendlyName = "Day 20",
          isDefault = True,
          run = do
            input <- readFileContents "app/day20_input.txt"
            let result = howManyPixelsAreLitExtended input 2
            print result
        }
    ),
    ( Day
        { name = "day19part02",
          friendlyName = "Day 19/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day19_input.txt"
            let result = largestManhattanDistance input
            print result
        }
    ),
    ( Day
        { name = "day19",
          friendlyName = "Day 19",
          isDefault = True,
          run = do
            input <- readFileContents "app/day19_input.txt"
            let result = howManyBeacons input
            print result
        }
    ),
    ( Day
        { name = "day18part02",
          friendlyName = "Day 18/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day18_input.txt"
            let result = largestMagnitude input
            print result
        }
    ),
    ( Day
        { name = "day18",
          friendlyName = "Day 18",
          isDefault = True,
          run = do
            input <- readFileContents "app/day18_input.txt"
            let result = finalSnailfishSum input
            print result
        }
    ),
    ( Day
        { name = "day17part02",
          friendlyName = "Day 17/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day17_input.txt"
            let result = howManyDifferentVelocities input
            print result
        }
    ),
    ( Day
        { name = "day17",
          friendlyName = "Day 17",
          isDefault = True,
          run = do
            input <- readFileContents "app/day17_input.txt"
            let result = bestShotMaximumHeight input
            print result
        }
    ),
    ( Day
        { name = "day16part02",
          friendlyName = "Day 16/Part0 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day16_input.txt"
            let result = evaluate input
            print result
        }
    ),
    ( Day
        { name = "day16",
          friendlyName = "Day 16",
          isDefault = True,
          run = do
            input <- readFileContents "app/day16_input.txt"
            let result = versionSum input
            print result
        }
    ),
    ( Day
        { name = "day15part02",
          friendlyName = "Day 15/Part02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day15_input.txt"
            let result = lowestTotalExtended input
            print result
        }
    ),
    ( Day
        { name = "day15",
          friendlyName = "Day 15",
          isDefault = True,
          run = do
            input <- readFileContents "app/day15_input.txt"
            let result = lowestTotal input
            print result
        }
    ),
    ( Day
        { name = "day14part02",
          friendlyName = "Day 14/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day14_input.txt"
            let result = polymerElementsExtended input 40
            print result
            putStr " --> "
            print (uncurry (-) result)
        }
    ),
    ( Day
        { name = "day14",
          friendlyName = "Day 14",
          isDefault = True,
          run = do
            input <- readFileContents "app/day14_input.txt"
            let result = polymerElements input
            print result
            putStr " --> "
            print (uncurry (-) result)
        }
    ),
    ( Day
        { name = "day13part02",
          friendlyName = "Day 13/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day13_input.txt"
            let result = printDots input
            printf "\n%s" result
        }
    ),
    ( Day
        { name = "day13",
          friendlyName = "Day 13",
          isDefault = True,
          run = do
            input <- readFileContents "app/day13_input.txt"
            let result = howManyDots input
            print result
        }
    ),
    ( Day
        { name = "day12part02",
          friendlyName = "Day 12/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day12_input.txt"
            let result = howManyPathsV2 input
            print result
        }
    ),
    ( Day
        { name = "day12",
          friendlyName = "Day 12",
          isDefault = True,
          run = do
            input <- readFileContents "app/day12_input.txt"
            let result = howManyPaths input
            print result
        }
    ),
    ( Day
        { name = "day11part02",
          friendlyName = "Day 11/ Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day11_input.txt"
            let result = flashSimultaneously input
            print result
        }
    ),
    ( Day
        { name = "day11",
          friendlyName = "Day 11",
          isDefault = True,
          run = do
            input <- readFileContents "app/day11_input.txt"
            let result = totalFlashes input 100
            print result
        }
    ),
    ( Day
        { name = "day10part02",
          friendlyName = "Day 10/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day10_input.txt"
            let result = middleScore input
            print result
        }
    ),
    ( Day
        { name = "day10",
          friendlyName = "Day 10",
          isDefault = True,
          run = do
            input <- readFileContents "app/day10_input.txt"
            let result = syntaxErrorScore input
            print result
        }
    ),
    ( Day
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
    ),
    ( Day
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
          isDefault = True,
          run = do
            input <- readFileContents "app/day06_input.txt"
            let result = simulateLanternfishExtended 256 input
            print result
        }
    ),
    ( Day
        { name = "day06",
          friendlyName = "Day 06",
          isDefault = True,
          run = do
            input <- readFileContents "app/day06_input.txt"
            let result = simulateLanternfish 80 input
            print result
        }
    ),
    ( Day
        { name = "day05part02",
          friendlyName = "Day 05/Part 02",
          isDefault = True,
          run = do
            input <- readFileContents "app/day05_input.txt"
            let result = overlapDiagonal input
            print result
        }
    ),
    ( Day
        { name = "day05",
          friendlyName = "Day 05",
          isDefault = True,
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
        printf "%s  not found.\n"
      | otherwise = runDay . head $ foundDays
      where
        foundDays = filter ((== input) . name) days

runAll :: IO ()
runAll = do
  (_, time) <- stopWatch (mapM_ runDay days)
  printf "Completed in %d.%0.9d seconds.\n" (sec time) (nsec time)

runDefault :: IO ()
runDefault = do
  (_, time) <- stopWatch (mapM_ runOrOmit days)
  printf "Completed in %d.%0.9d seconds.\n" (sec time) (nsec time)
  where
    runOrOmit :: Day -> IO ()
    runOrOmit day
      | isDefault day = runDay day
      | otherwise = omitDay day

runDay :: Day -> IO ()
runDay day = do
  printf "Running %s (%s) ...\n" (friendlyName day) (name day)
  putStr " --> "
  (_, time) <- stopWatch (run day)
  printf "Finished. (%d.%0.9d seconds)\n" (sec time) (nsec time)

omitDay :: Day -> IO ()
omitDay day = do
  printf "%s (%s) omitted.\n" (friendlyName day) (name day)

readFileContents :: String -> IO String
readFileContents filename = do
  fileHandle <- openFile filename ReadMode
  hGetContents fileHandle

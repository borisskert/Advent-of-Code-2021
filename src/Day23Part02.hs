{-# LANGUAGE TupleSections #-}

module Day23Part02 where

-- https://adventofcode.com/2021/day/23#part2

import Data.List (intercalate)
import Data.Map (fromList)
import Day23 (Edges, parseInput, search)

extendInput :: String -> String
extendInput input =
  intercalate "\n"
    . (++ tailInputLines)
    . (++ extensionLines)
    . take insertLineIndex
    $ inputLines
  where
    insertLineIndex = 3
    extension = "  #D#C#B#A#\n  #D#B#A#C#\n"
    extensionLines = lines extension :: [String]
    inputLines = lines input :: [String]
    tailInputLines = drop insertLineIndex inputLines

leastEnergyRequiredExtended :: String -> Int
leastEnergyRequiredExtended = search . (,edges) . parseInput . extendInput

edges =
  fromList
    [ (('H', 0), [('H', 1)]),
      (('H', 1), [('H', 0), ('H', 2)]),
      (('H', 2), [('H', 1), ('H', 3), ('A', 0)]),
      (('H', 3), [('H', 2), ('H', 4)]),
      (('H', 4), [('H', 3), ('H', 5), ('B', 0)]),
      (('H', 5), [('H', 4), ('H', 6)]),
      (('H', 6), [('H', 5), ('H', 7), ('C', 0)]),
      (('H', 7), [('H', 6), ('H', 8)]),
      (('H', 8), [('H', 7), ('H', 9), ('D', 0)]),
      (('H', 9), [('H', 8), ('H', 10)]),
      (('H', 10), [('H', 9)]),
      (('A', 0), [('A', 1), ('H', 2)]),
      (('A', 1), [('A', 0), ('A', 2)]),
      (('A', 2), [('A', 1), ('A', 3)]),
      (('A', 3), [('A', 2)]),
      (('B', 0), [('B', 1), ('H', 4)]),
      (('B', 1), [('B', 0), ('B', 2)]),
      (('B', 2), [('B', 1), ('B', 3)]),
      (('B', 3), [('B', 2)]),
      (('C', 0), [('C', 1), ('H', 6)]),
      (('C', 1), [('C', 0), ('C', 2)]),
      (('C', 2), [('C', 1), ('C', 3)]),
      (('C', 3), [('C', 2)]),
      (('D', 0), [('D', 1), ('H', 8)]),
      (('D', 1), [('D', 0), ('D', 2)]),
      (('D', 2), [('D', 1), ('D', 3)]),
      (('D', 3), [('D', 2)])
    ] ::
    Edges

module Day15 where

-- https://adventofcode.com/2021/day/15

import Data.Char (digitToInt)
import Data.Map (Map, insert, empty, toList, notMember, delete)
import Data.List (sortOn)

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

type RiskLevel = Int

type Field = [[RiskLevel]]

type Position = (Int, Int)

type Point = (Position, RiskLevel)

type Points = Map Position RiskLevel

riskLevelAt :: Position -> Field -> RiskLevel
riskLevelAt position = (!! x) . (!! y)
  where
    (x, y) = position

insertAll :: [Point] -> Points -> Points
insertAll points field = foldl (flip (uncurry insert)) field points

fieldOf :: Point -> Points
fieldOf point = insert position value empty
  where
    (position, value) = point

lowestRiskLevel :: Field -> RiskLevel
lowestRiskLevel field = findPath empty (fieldOf start)
  where
    size = length field :: Int
    start = ((0, 0), 0) :: Point

    findPath :: Points -> Points -> RiskLevel
    findPath visited path
      | bestPointPosition == (size - 1, size - 1) = bestPointValue
      | null neighbors = findPath nextVisited pathWithout
      | otherwise = findPath nextVisited nextPath
      where
        bestPoint = head . sortOn snd . toList $ path :: Point
        (bestPointPosition, bestPointValue) = bestPoint
        neighbors = filter (`notMember` path) . filter (`notMember` visited) . neighborsOf $ bestPointPosition :: [Position]
        nextPathMembers = map (\p -> (p, (p `riskLevelAt` field) + bestPointValue)) neighbors :: [Point]
        nextVisited = insert bestPointPosition bestPointValue visited :: Points
        pathWithout = delete bestPointPosition path :: Points
        nextPath = insertAll nextPathMembers pathWithout :: Points

    neighborsOf :: Position -> [Position]
    neighborsOf position = right ++ left ++ top ++ bottom
      where
        (x, y) = position
        right
          | x < size - 1 = [(x + 1, y)]
          | otherwise = []
        left
          | x > 0 = [(x - 1, y)]
          | otherwise = []
        top
          | y > 0 = [(x, y - 1)]
          | otherwise = []
        bottom
          | y < size - 1 = [(x, y + 1)]
          | otherwise = []

lowestTotal :: String -> Int
lowestTotal = lowestRiskLevel . parseInput

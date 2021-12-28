module Day15Part02 where

import Data.List (sortOn)
import Data.Map (Map, delete, empty, insert, member, notMember, toList)
import Day15 (parseInput)
import Debug.Trace (traceShow)

-- https://adventofcode.com/2021/day/15#part2

type RiskLevel = Int

type Field = [[RiskLevel]]

type Position = (Int, Int)

type Point = (Position, RiskLevel)

type Points = Map Position RiskLevel

fieldOf :: Point -> Points
fieldOf point = insert position value empty
  where
    (position, value) = point

expand :: Field -> Field
expand values = map (rowinger 0) values ++ map (rowinger 1) values ++ map (rowinger 2) values ++ map (rowinger 3) values ++ map (rowinger 4) values
  where
    rowinger :: Int -> [Int] -> [Int]
    rowinger i x = map (offset i) x ++ map (offset (i + 1)) x ++ map (offset (i + 2)) x ++ map (offset (i + 3)) x ++ map (offset (i + 4)) x

    offset :: Int -> Int -> Int
    offset i x
      | x + i > 9 = x + i + 1 - 10
      | otherwise = x + i

riskLevelAt :: Position -> Field -> RiskLevel
riskLevelAt position = (!! x) . (!! y)
  where
    (x, y) = position

insertAll :: [Point] -> Points -> Points
insertAll points field = foldl (flip (uncurry insert)) field points

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

lowestTotalExtended :: String -> Int
lowestTotalExtended = lowestRiskLevel . expand . parseInput

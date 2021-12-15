module Day15Part02 where

import Data.List (nub, sortOn)
import Data.Map (Map, elems, insert)
import Data.Ord (Down (Down))
import Day15 (Field (Field, points, size), Point (Point, distance, neighbors, position, riskLevel), Position (Position, x, y), findBestRoute, lookupPoint, parseInput, riskLevelAtStart, riskLevelOf, toField, Size (height, width))

extend :: [[Int]] -> [[Int]]
extend values = map (rowinger 0) values ++ map (rowinger 1) values ++ map (rowinger 2) values ++ map (rowinger 3) values ++ map (rowinger 4) values
  where
    rowinger :: Int -> [Int] -> [Int]
    rowinger i x = map (offset i) x ++ map (offset (i + 1)) x ++ map (offset (i + 2)) x ++ map (offset (i + 3)) x ++ map (offset (i + 4)) x

    offset :: Int -> Int -> Int
    offset i x
      | x + i > 9 = x + i + 1 - 10
      | otherwise = x + i

data Distance = Distance Int deriving (Eq, Show)

addDistance :: Point -> Int -> Point
addDistance Point {position = position, riskLevel = risklevel, neighbors = neigbors} distance = (Point {position = position, riskLevel = risklevel, neighbors = neigbors, distance = risklevel + distance})

updatePoint :: Field -> Point -> Field
updatePoint Field {size = s, points = ps} p = Field {size = s, points = updatedPoints}
  where
    updatedPoints = insert (position p) p $ ps :: Map Position Point

updatePoints :: Field -> [Point] -> Field
updatePoints = foldl updatePoint

initDistance :: Field -> Field
initDistance field
  | null distancedPoints = visit [start]
  | not . null $ anyNotVisitedPoints = visit anyNotVisitedPoints
  | otherwise = field
  where
    allPoints = elems . points $ field :: [Point]
    distancedPoints = filter visited allPoints :: [Point]
    startPosition = Position {x = 0, y = 0} :: Position
    start = lookupPoint field startPosition :: Point

    otherPoints = filter (/= start) allPoints :: [Point]
    anyNotVisitedPoints = filter (not . visited) otherPoints :: [Point]

    visit :: [Point] -> Field
    visit notVisited =
      initDistance
        . updatePoints field
        . nub
        . concatMap visitNeighborsOf
        . sortOn (Down . distance)
        . filter visited
        . map (lookupPoint field)
        . nub
        . concatMap neighbors
        $ notVisited
      where
        visitNeighborsOf :: Point -> [Point]
        visitNeighborsOf point = map (`addDistance` distance point) . filter (not . visited) . map (lookupPoint field) . neighbors $ point

    visited :: Point -> Bool
    visited p = ((> 0) . distance $ p) || (p == start)

lowestTotalExtended :: String -> Int
lowestTotalExtended input = (+ (- riskLevelStart)) . riskLevelOf . findBestRoute $ field
  where
    field = toField . extend . parseInput $ input :: Field
    riskLevelStart = riskLevelAtStart field :: Int

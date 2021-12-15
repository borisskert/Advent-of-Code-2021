module Day15 where

import Data.Char (digitToInt)
import Data.List (nub, sortOn)
import Data.Map (Map, drop, fromList, keys, lookup)
import Data.Maybe (fromJust)
import Day12 (isFinished)
import Prelude hiding (lookup)

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

data Size = Size {width :: Int, height :: Int} deriving (Eq, Show)

data Field = Field {size :: Size, points :: Map Position Point} deriving (Eq, Show)

data Position = Position {x :: Int, y :: Int} deriving (Eq, Show, Ord)

data Point = Point {position :: Position, riskLevel :: Int, neighbors :: [Position]} deriving (Eq, Show)

toField :: [[Int]] -> Field
toField values = (\m -> Field {size = size, points = m}) . fromList . mapFromInts 0 $ values
  where
    size = Size {width = length . head $ values, height = length values} :: Size

    mapFromInts :: Int -> [[Int]] -> [(Position, Point)]
    mapFromInts _ [] = []
    mapFromInts i (line : lines) = points ++ mapFromInts (i + 1) lines
      where
        points =
          map
            ( (\pos -> (pos, Point {position = pos, riskLevel = line !! x pos, neighbors = makeNeighbors size pos}))
                . (\x -> (Position {x = x, y = i}))
            )
            [0 .. (width size - 1)] ::
            [(Position, Point)]

    makeNeighbors :: Size -> Position -> [Position]
    makeNeighbors size pos =
      map toPosition
        . filter (\(x, y) -> x >= 0 && y >= 0)
        . filter (\(x, y) -> x < width size && y < height size)
        . filter (\(x, y) -> not (x == posX && y == posY))
        $ [(posX -1, posY), (posX, posY -1), (posX, posY + 1), (posX + 1, posY)]
      where
        posX = x pos
        posY = y pos

        toPosition :: (Int, Int) -> Position
        toPosition (x, y) = Position {x = x, y = y}

lookupPoint :: Field -> Position -> Point
lookupPoint Field {size = _, points = points} position = fromJust . lookup position $ points

data Route = Route {route :: [Point], visited :: [Position]} deriving (Eq, Show)

lastOf :: Route -> Point
lastOf Route {route = points} = last points

riskLevelOf :: Route -> Int
riskLevelOf Route {route = points} = sum . map riskLevel $ points

isOnRoute :: Route -> Position -> Bool
isOnRoute Route {route = points} pos = any ((== pos) . position) points

isOnAnyRoute :: [Route] -> Position -> Bool
isOnAnyRoute routes pos = any (`isOnRoute` pos) routes

isEmpty :: Route -> Bool
isEmpty Route {route = points} = null points

newRoute :: Point -> Route
newRoute point = Route {route = [point], visited = []}

getRouteNeighbors :: Route -> [Position]
getRouteNeighbors = neighbors . lastOf

getRoutesNeighbors :: [Route] -> [Position]
getRoutesNeighbors routes = nub . filter (not . isOnAnyRoute routes) . concatMap getRouteNeighbors $ routes

hasNeighbor :: Route -> Position -> Bool
hasNeighbor route pos = (pos `elem`) . getRouteNeighbors $ route

hasAnyNeighbors :: Route -> Bool
hasAnyNeighbors = null . getRouteNeighbors

neighboredRoutes :: [Route] -> Position -> [Route]
neighboredRoutes routes pos = filter (`hasNeighbor` pos) routes

bestRouteTo :: [Route] -> Position -> Route
bestRouteTo routes pos = head . sortOn riskLevelOf $ neighboredRoutes routes pos

risklevels :: Route -> [Int]
risklevels Route {route = route} = map riskLevel route

lowestTotal :: String -> Int
lowestTotal input = (+ (- riskLevelStart)) . minimum . map riskLevelOf $ foundRoutes
  where
    field = toField . parseInput $ input :: Field
    sizeOfField = size field :: Size
    startPosition = Position {x = 0, y = 0} :: Position
    start = lookupPoint field startPosition :: Point
    riskLevelStart = riskLevel start :: Int

    endPosition = Position {x = width sizeOfField - 1, y = height sizeOfField - 1}

    initialRoute = newRoute start :: Route

    foundRoutes = head . map finishedRoutes . dropWhile hasOpenRoutes . iterate extendRoutes $ ([initialRoute], [], []) :: [Route]

    hasOpenRoutes :: ([Route], [Route], [Route]) -> Bool
    hasOpenRoutes (o, _, _) = not . null $ o

    finishedRoutes :: ([Route], [Route], [Route]) -> [Route]
    finishedRoutes (_, f, _) = f

    append :: Route -> Position -> Route
    append Route {route = points, visited = visited} position = Route {route = points ++ [point], visited = visited}
      where
        point = lookupPoint field position

    extendRoutes :: ([Route], [Route], [Route]) -> ([Route], [Route], [Route])
    extendRoutes (openRoutes, finishedRoutes, stuckRoutes) = (newOpenRoutes, newFinishedRoutes, newStuckRoutes)
      where
        potentialNeigbors = filter (not . isOnAnyRoute (finishedRoutes ++ stuckRoutes)) . getRoutesNeighbors $ openRoutes :: [Position]

        newRoutes = map extendToRoute . filter (not . isOnAnyRoute (openRoutes ++ finishedRoutes ++ stuckRoutes)) $ potentialNeigbors :: [Route]

        newOpenRoutes = filter (not . isFinished) newRoutes :: [Route]
        newFinishedRoutes = (finishedRoutes ++) . filter isFinished $ newRoutes :: [Route]
        newStuckRoutes = (stuckRoutes ++) . filter isStuck $ newRoutes :: [Route]

        extendToRoute :: Position -> Route
        extendToRoute pos = (`append` pos) $ bestRouteTo openRoutes pos

        isStuck :: Route -> Bool
        isStuck = all (isOnAnyRoute otherRoutes) . getRouteNeighbors
          where
            otherRoutes = newOpenRoutes ++ newFinishedRoutes ++ stuckRoutes :: [Route]

    isFinished :: Route -> Bool
    isFinished route = isOnRoute route endPosition

    areFinished :: [Route] -> Bool
    areFinished = all isFinished

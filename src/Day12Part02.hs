module Day12Part02 where

-- https://adventofcode.com/2021/day/12#part2

import Data.List (nub)
import Day12 (Cave (Cave), Connection (Connection), Path (Path), current, ending, find, isBig, isEnd, isFinished, isStart, parseInput, startFrom, toCaves)

toConnections :: [(String, String)] -> [Connection]
toConnections = concatMap expand
  where
    expand :: (String, String) -> [Connection]
    expand (s, t)
      | isStart source = [Connection (source, target)]
      | isStart target = [Connection (target, source)]
      | isEnd target = [Connection (source, target)]
      | isStart target = [Connection (target, source)]
      | otherwise = [Connection (source, target), Connection (target, source)]
      where
        source = Cave s :: Cave
        target = Cave t :: Cave

findPaths :: [(String, String)] -> [Path]
findPaths input = Path [start] `extendBy` connections
  where
    caves = toCaves input :: [Cave]
    start = find isStart caves :: Cave
    connections = toConnections input :: [Connection]

extendBy :: Path -> [Connection] -> [Path]
extendBy path connections
  | isFinished path = [path]
  | otherwise = concatMap (`extendBy` connections) . extendPath $ path
  where
    extendPath :: Path -> [Path]
    extendPath path = appendAll endingCaves $ path
      where
        cave = current path :: Cave
        fromCave = filter (startFrom cave) connections :: [Connection]
        endingCaves = map ending fromCave :: [Cave]

append :: Cave -> Path -> Path
append cave (Path caves)
  | isBig cave = Path (caves ++ [cave])
  | isStart cave = Path caves
  | (> 2) . length . filter (== cave) $ caves = Path caves
  | otherwise = Path (caves ++ [cave])

appendAll :: [Cave] -> Path -> [Path]
appendAll caves path = filter (/= path) . filter isAllowed . map (`append` path) $ caves

isAllowed :: Path -> Bool
isAllowed (Path caves)
  | any (> 2) counts = False
  | otherwise = (< 2) . length . filter (== 2) $ counts
  where
    counts = map count . nub $ caves :: [Int]

    count :: Cave -> Int
    count cave
      | isBig cave = 0
      | otherwise = length . filter (== cave) $ caves

howManyPathsV2 :: String -> Int
howManyPathsV2 = length . findPaths . parseInput

module Day12 where

-- https://adventofcode.com/2021/day/12

import Data.Char (isUpper)
import Data.List (nub)
import Data.List.Split (splitOn)

data Cave = Cave String deriving (Eq, Show)

data Connection = Connection (Cave, Cave) deriving (Eq, Show)

data Path = Path [Cave] deriving (Eq, Show)

parseInput :: String -> [(String, String)]
parseInput = map ((\x -> (head x, last x)) . splitOn "-") . lines

findPaths :: [(String, String)] -> [Path]
findPaths input = Path [start] `extendBy` connections
  where
    caves = toCaves input :: [Cave]
    start = find isStart caves :: Cave
    connections = toConnections input :: [Connection]

toCaves :: [(String, String)] -> [Cave]
toCaves = nub . map toCave . nub . concatMap (\(left, right) -> [left, right])

toCave :: String -> Cave
toCave = Cave

extendBy :: Path -> [Connection] -> [Path]
extendBy path connections
  | isFinished path = [path]
  | otherwise = nub . concatMap (`extendBy` connections) . extendPath $ path
  where
    extendPath :: Path -> [Path]
    extendPath path = appendAll endingCaves path
      where
        cave = current path :: Cave
        fromCave = filter (startFrom cave) connections :: [Connection]
        endingCaves = map ending fromCave :: [Cave]

find :: (Eq a) => (a -> Bool) -> [a] -> a
find f xs
  | null found = error "Cannot find element"
  | otherwise = head found
  where
    found = filter f xs

append :: Cave -> Path -> Path
append cave (Path caves)
  | cave `elem` caves && (not . isBig $ cave) = Path caves
  | otherwise = Path (caves ++ [cave])

appendAll :: [Cave] -> Path -> [Path]
appendAll caves path = filter (/= path) . map (`append` path) $ caves

toConnections :: [(String, String)] -> [Connection]
toConnections input = cleanSackgassen connections
  where
    connections = nub . concatMap expand $ input :: [Connection]

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

    cleanSackgassen :: [Connection] -> [Connection]
    cleanSackgassen cs
      | cs == cleaned = cleaned
      | otherwise = cleanSackgassen cleaned
      where
        cleaned = filter (not . isSackgasse) cs

    isSackgasse :: Connection -> Bool
    isSackgasse (Connection (source, target))
      | isBig source || isBig target = False
      | (== 1) . length . connectedCaves connections $ source = True
      | (== 1) . length . connectedCaves connections $ target = True
      | otherwise = False

connectedCaves :: [Connection] -> Cave -> [Cave]
connectedCaves connections cave = filter (/= cave) . toCaves . filter (\(Connection (source, target)) -> source == cave || target == cave) $ connections
  where
    toCaves :: [Connection] -> [Cave]
    toCaves = nub . concatMap (\(Connection (source, target)) -> [source, target])

current :: Path -> Cave
current (Path caves) = last caves

contains :: Path -> Cave -> Bool
contains (Path caves) cave = cave `elem` caves

startFrom :: Cave -> Connection -> Bool
startFrom cave (Connection (source, _)) = cave == source

endsAt :: Cave -> Connection -> Bool
endsAt cave (Connection (_, target)) = cave == target

ending :: Connection -> Cave
ending (Connection (_, cave)) = cave

isFinished :: Path -> Bool
isFinished (Path []) = False
isFinished (Path caves) = (== Cave "end") . last $ caves

isStart :: Cave -> Bool
isStart (Cave "start") = True
isStart _ = False

isEnd :: Cave -> Bool
isEnd (Cave "end") = True
isEnd _ = False

isBig :: Cave -> Bool
isBig (Cave c) = all isUpper c

howManyPaths :: String -> Int
howManyPaths = length . findPaths . parseInput

module Day11 where

-- https://adventofcode.com/2021/day/11

import Data.Char (digitToInt)
import Data.Maybe (fromJust, isNothing)

data Position = Position {x :: Int, y :: Int} deriving (Eq, Show)

data Octopus = Octopus {position :: Position, energy :: Int} deriving (Eq, Show)

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

octopuses :: [[Int]] -> [Octopus]
octopuses xss = concatMap mapLine . zip [0 .. (sizeY -1)] $ xss
  where
    sizeY = length xss :: Int

    mapLine :: (Int, [Int]) -> [Octopus]
    mapLine (y, xs) = zipWith (curry mapValue) [0 .. (sizeX - 1)] xs
      where
        sizeX = length xs :: Int

        mapValue :: (Int, Int) -> Octopus
        mapValue (x, energy) = Octopus {position = Position {x = x, y = y}, energy = energy}

step :: [Octopus] -> [Octopus]
step all =
  merge increasedEnergy
    . foldl flash increasedEnergy
    . filter isFlashing
    $ increasedEnergy
  where
    increasedEnergy = map increaseEnergy all :: [Octopus]

flash :: [Octopus] -> Octopus -> [Octopus]
flash [] _ = []
flash all x = foldl (\a o -> merge a . flash a $ o) allPLUS flashingAdjacents
  where
    adjacents = filter (isAdjacent x) all :: [Octopus]
    increasedAdjacents = map increaseEnergy . filter (not . isFlashing) $ adjacents :: [Octopus]
    flashingAdjacents = filter isFlashing increasedAdjacents :: [Octopus]
    allPLUS = merge (merge all increasedAdjacents) flashingAdjacents

find :: (Eq a) => (a -> Bool) -> [a] -> Maybe a
find f xs
  | null found = Nothing
  | otherwise = Just . head $ found
  where
    found = filter f xs

merge :: [Octopus] -> [Octopus] -> [Octopus]
merge old new = map selectNew old
  where
    selectNew :: Octopus -> Octopus
    selectNew Octopus {position = pos, energy = e}
      | isNothing newOne = Octopus {position = pos, energy = e}
      | otherwise = Octopus {position = pos, energy = energy . fromJust $ newOne}
      where
        newOne = find (hasPosition pos) new :: Maybe Octopus

hasPosition :: Position -> Octopus -> Bool
hasPosition otherPos Octopus {position = pos, energy = _} = pos == otherPos

increaseEnergy :: Octopus -> Octopus
increaseEnergy Octopus {position = Position {x = x, y = y}, energy = energy} = (Octopus {position = Position {x = x, y = y}, energy = energy + 1})

resetEnergyIfFlashing :: Octopus -> Octopus
resetEnergyIfFlashing x
  | isFlashing x = resetEnergy x
  | otherwise = x

resetEnergy :: Octopus -> Octopus
resetEnergy Octopus {position = Position {x = x, y = y}, energy = _} = (Octopus {position = Position {x = x, y = y}, energy = 0})

isFlashing :: Octopus -> Bool
isFlashing Octopus {position = _, energy = energy} = energy > 9

isAdjacent :: Octopus -> Octopus -> Bool
isAdjacent Octopus {position = Position {x = x1, y = y1}, energy = _} Octopus {position = Position {x = x2, y = y2}, energy = _}
  | x1 == x2 && y1 == y2 = False
  | otherwise = abs (x1 - x2) < 2 && abs (y1 - y2) < 2

totalFlashes :: String -> Int -> Int
totalFlashes input steps =
  sum
    . map fst
    . scanl (\(_, os) i -> counting os) (0, all)
    $ [1 .. steps]
  where
    all = octopuses . parseInput $ input :: [Octopus]

    counting :: [Octopus] -> (Int, [Octopus])
    counting os = (countFlashes next, map resetEnergyIfFlashing next)
      where
        next = step os

countFlashes :: [Octopus] -> Int
countFlashes = length . filter isFlashing

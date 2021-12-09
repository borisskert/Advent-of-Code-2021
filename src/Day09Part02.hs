module Day09Part02 where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.Map (Map, fromList, lookup, toList)
import Data.Maybe (fromJust, isNothing)
import Prelude hiding (lookup)

-- https://adventofcode.com/2021/day/9#part2

data Size = Size {width :: Int, height :: Int} deriving (Eq, Show)

data Field = Field {size :: Size, points :: Map Position Point} deriving (Eq, Show)

data Position = Position {x :: Int, y :: Int} deriving (Eq, Show, Ord)

data Point = Point {position :: Position, value :: Int, neighbors :: [Position]} deriving (Eq, Show)

data Basin = Basin [Position] deriving (Eq, Show)

parseInput :: String -> Field
parseInput input = (\m -> Field {size = size, points = m}) . fromList . mapFromInts 0 $ values
  where
    values = map (map digitToInt) . lines $ input :: [[Int]]
    size = Size {width = length . head $ values, height = length values} :: Size

    mapFromInts :: Int -> [[Int]] -> [(Position, Point)]
    mapFromInts _ [] = []
    mapFromInts i (line : lines) = points ++ mapFromInts (i + 1) lines
      where
        points =
          map
            ( (\pos -> (pos, Point {position = pos, value = line !! x pos, neighbors = makeNeighbors size pos}))
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

find :: Field -> Position -> Point
find field pos
  | isNothing point = error ("Cannot find: " ++ show pos)
  | otherwise = fromJust point
  where
    point = lookup pos . points $ field :: Maybe Point

basinFrom :: Field -> Position -> Basin
basinFrom field pos = collect (Basin []) [pos]
  where
    collect :: Basin -> [Position] -> Basin
    collect basin [] = basin
    collect (Basin basin) (p : positions)
      | p `elem` basin = collect (Basin basin) positions
      | otherwise = collect (Basin (p : basin)) nextPositions
      where
        nextPositions = positions ++ neighborPositions
        neighborPositions =
          filter (`notElem` basin)
            . filter ((< 9) . value . find field)
            . neighbors
            $ find field p

basinSize :: Basin -> Int
basinSize (Basin xs) = length xs

lowPoints :: Field -> [Point]
lowPoints field = filter isLowPoint . map snd . toList . points $ field
  where
    isLowPoint :: Point -> Bool
    isLowPoint p = all (((> value p) . value) . find field) $ neighbors p

largestBasins :: String -> [Int]
largestBasins input =
  take 3
    . reverse
    . sort
    . map
      ( basinSize
          . basinFrom field
          . position
      )
    . lowPoints
    $ field
  where
    field = parseInput input :: Field

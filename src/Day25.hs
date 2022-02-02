{-# LANGUAGE TupleSections #-}

module Day25 where

-- https://adventofcode.com/2021/day/25

import Data.Bifunctor (second)
import Data.List (intercalate)
import Data.Map (Map, delete, fromList, insert, keys, lookup, toList)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

type Position = (Int, Int)

type Direction = Position -> Position

type SeaCucumber = Char

type Grid = Map Position SeaCucumber

type Size = (Int, Int)

type SeaCucumbers = (Size, Grid)

parseToGrid :: (Eq a) => [[Char]] -> ((Position, Char) -> Maybe a) -> Map Position a
parseToGrid inputLines toValue =
  fromList
    . map (second fromJust)
    . filter (isJust . snd)
    . map (\(p, c) -> (p, toValue (p, c)))
    . concatMap (uncurry toLine)
    . zip [0 ..]
    $ inputLines
  where
    toLine :: Int -> [Char] -> [(Position, Char)]
    toLine y line = zipWith (\x c -> ((x, y), c)) [0 ..] line

gridToString :: (Eq a) => (Int, Int) -> Map Position a -> (Maybe a -> Char) -> [Char]
gridToString size grid valueToChar =
  intercalate "\n"
    . map (\y -> map (valueToChar . (`lookup` grid) . (,y)) xs)
    $ ys
  where
    (width, height) = size
    positions = keys grid :: [Position]
    minX = 0
    minY = 0
    maxX = width -1
    maxY = height - 1
    xs = [minX, (minX + 1) .. maxX]
    ys = [minY, (minY + 1) .. maxY]
    allPositions = [(x, y) | x <- xs, y <- ys] :: [Position]

toString :: SeaCucumbers -> String
toString cucumbers = gridToString size grid cucumberToString
  where
    (size, grid) = cucumbers

    cucumberToString :: Maybe SeaCucumber -> Char
    cucumberToString Nothing = '.'
    cucumberToString (Just c) = c

eastOf :: Size -> Position -> Position
eastOf size position = (x', y)
  where
    (width, height) = size
    (x, y) = position

    x'
      | x + 1 >= width = 0
      | otherwise = x + 1

southOf :: Size -> Position -> Position
southOf size position = (x, y')
  where
    (width, height) = size
    (x, y) = position

    y'
      | y + 1 >= height = 0
      | otherwise = y + 1

parseToSize :: [[Char]] -> Size
parseToSize lines = (length . head $ lines, length lines)

parseInput :: String -> SeaCucumbers
parseInput input = (size, grid)
  where
    inputLines = lines input :: [[Char]]

    size = parseToSize inputLines :: Size
    grid = parseToGrid inputLines toCucumber :: Grid

    toCucumber :: (Position, Char) -> Maybe SeaCucumber
    toCucumber (position, char)
      | char == '>' = Just char
      | char == 'v' = Just char
      | otherwise = Nothing

isMovingEast :: SeaCucumber -> Bool
isMovingEast = (== '>')

isMovingSouth :: SeaCucumber -> Bool
isMovingSouth = (== 'v')

canMoveEast :: Position -> SeaCucumbers -> Bool
canMoveEast position cucumbers = isNothing neighbor
  where
    (size, grid) = cucumbers
    newPosition = eastOf size position

    neighbor = lookup newPosition grid :: Maybe SeaCucumber

canMoveSouth :: Position -> SeaCucumbers -> Bool
canMoveSouth position cucumbers = isNothing neighbor
  where
    (size, grid) = cucumbers
    newPosition = southOf size position

    neighbor = lookup newPosition grid :: Maybe SeaCucumber

move :: Position -> SeaCucumbers -> SeaCucumbers
move position cucumbers
  | isNothing neighbor = (size, newGrid)
  | otherwise = cucumbers
  where
    (size, grid) = cucumbers

    cucumber = fromJust . lookup position $ grid :: SeaCucumber
    neighbor = lookup newPosition grid :: Maybe SeaCucumber

    newPosition
      | isMovingEast cucumber = eastOf size position
      | isMovingSouth cucumber = southOf size position
      | otherwise = error "Should not happen"

    newGrid = insert newPosition cucumber . delete position $ grid

moveEast :: SeaCucumbers -> SeaCucumbers
moveEast cucumbers = foldl (flip move) cucumbers movingEast
  where
    (size, grid) = cucumbers
    movingEast = filter (`canMoveEast` cucumbers) . map fst . filter (isMovingEast . snd) . toList $ grid :: [Position]

moveSouth :: SeaCucumbers -> SeaCucumbers
moveSouth cucumbers = foldl (flip move) cucumbers movingSouth
  where
    (size, grid) = cucumbers
    movingSouth = filter (`canMoveSouth` cucumbers) . map fst . filter (isMovingSouth . snd) . toList $ grid :: [Position]

step :: SeaCucumbers -> SeaCucumbers
step = moveSouth . moveEast

steps :: SeaCucumbers -> (SeaCucumbers, Int)
steps = go 0
  where
    go :: Int -> SeaCucumbers -> (SeaCucumbers, Int)
    go counter cucumbers
      | cucumbers == next = (cucumbers, counter + 1)
      | otherwise = go (counter + 1) next
      where
        next = step cucumbers

stepsUntilDeadlock :: String -> Int
stepsUntilDeadlock input = count
  where
    cucumbers = parseInput input :: SeaCucumbers
    (_, count) = steps cucumbers

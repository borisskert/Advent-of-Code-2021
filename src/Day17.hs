module Day17 where

-- https://adventofcode.com/2021/day/17

import Data.List (maximumBy, sort, sortOn)
import Data.List.Split (splitOn)

parseInput :: String -> ((Int, Int), (Int, Int))
parseInput = tupleFromList . map toTuple . splitOn ", " . dropWhile (/= 'x')
  where
    toTuple :: String -> (Int, Int)
    toTuple = tupleFromList . map read . splitOn ".." . last . splitOn "="

    tupleFromList :: [a] -> (a, a)
    tupleFromList (x1 : x2 : _) = (x1, x2)

data TargetArea = TargetArea (Position, Position) deriving (Eq, Show)

data Position = Position (Int, Int) deriving (Eq, Show)

data Velocity = Velocity (Int, Int) deriving (Eq, Show)

data Direction = Direction (Int, Int) deriving (Eq, Show)

targetArea :: ((Int, Int), (Int, Int)) -> TargetArea
targetArea ((x1, x2), (y1, y2)) = TargetArea (Position (x1, y2), Position (x2, y1))

isWithin :: Position -> TargetArea -> Bool
isWithin (Position (px, py)) (TargetArea (Position (x1, y2), Position (x2, y1))) =
  px <= x2 && px >= x1 && py <= y2 && py >= y1

isBeyond :: Position -> TargetArea -> Bool
isBeyond (Position (px, py)) (TargetArea (Position (x1, y1), Position (x2, y2))) =
  px > x2 || py < y2

shot :: TargetArea -> Velocity -> [Position]
shot target velocity = shotStep target start velocity
  where
    start = Position (0, 0)

    shotStep :: TargetArea -> Position -> Velocity -> [Position]
    shotStep target position velocity
      | position `isBeyond` target = [position]
      | position `isWithin` target = [position]
      | otherwise = position : shotStep target newPosition newVelocity
      where
        (px, py) = (\(Position (x, y)) -> (fromIntegral x, fromIntegral y)) position
        (vx, vy) = (\(Velocity (x, y)) -> (x, y)) velocity
        newPosition = Position (px + vx, py + vy) :: Position
        vxNew
          | vx < 0 = vx + 1
          | vx > 0 = vx -1
          | otherwise = vx
        newVelocity = Velocity (vxNew, vy - 1)

targetVelocity :: TargetArea -> Velocity
targetVelocity (TargetArea (Position (x1, y2), Position (x2, y1))) = Velocity (ceiling x, y)
  where
    y = - y1 :: Int
    y' = 0.5 + sqrt (0.25 + 2 * fromIntegral x2) :: Double
    x = sqrt (2 * (fromIntegral x2 - y')) :: Double

hasHit :: TargetArea -> [Position] -> Bool
hasHit target = any (`isWithin` target)

maximumHeight :: [Position] -> Int
maximumHeight =
  maximum
    . map getY
  where
    getY :: Position -> Int
    getY (Position (x, y)) = y

bestShot :: TargetArea -> [Position]
bestShot target = last . sortOn maximumHeight . filter (hasHit target) . map (shot target) $ velocities
  where
    velocity = targetVelocity target :: Velocity

    velocities = map Velocity . (\(xs, ys) -> [(x, y) | x <- xs, y <- ys]) . (\(Velocity (x, y)) -> ([0 .. x], [0 .. y])) $ velocity :: [Velocity]

bestShotMaximumHeight :: String -> Int
bestShotMaximumHeight = maximumHeight . bestShot . targetArea . parseInput

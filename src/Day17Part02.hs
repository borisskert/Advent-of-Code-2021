module Day17Part02 where

-- https://adventofcode.com/2021/day/17#part2

import Day17 (Position (Position), TargetArea (TargetArea), Velocity (Velocity), hasHit, parseInput, shot, targetArea)

testingVelocities :: TargetArea -> [Velocity]
testingVelocities (TargetArea (Position (x1, y1), Position (x2, y2))) =
  [Velocity (x, y) | x <- [0 .. x2], y <- [y2 .. (- y2)]]

howManyDifferentVelocities :: String -> Int
howManyDifferentVelocities input = length . filter (hasHit target) . map (shot target) $ velocities
  where
    target = targetArea . parseInput $ input :: TargetArea
    velocities = testingVelocities target :: [Velocity]

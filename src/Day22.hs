{-# LANGUAGE TupleSections #-}

module Day22 where

-- https://adventofcode.com/2021/day/22

import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Prelude hiding (lookup)

type CuboidValue = (Int, Int)

type Cuboid = (CuboidValue, CuboidValue, CuboidValue)

type Cuboids = [Cuboid]

data Geometry = Simple Cuboid | Without Cuboid Geometries deriving (Eq, Show)

type Geometries = [Geometry]

type RebootStep = (String, Cuboid)

type RebootSteps = [RebootStep]

type EnabledGeometry = (Bool, Geometry)

type EnabledGeometries = [EnabledGeometry]

parseInput :: String -> RebootSteps
parseInput = map parseLine . lines

parseLine :: String -> RebootStep
parseLine input = (head splitLine, values)
  where
    splitLine = splitOn " " input :: [String]
    values = parseValues . last $ splitLine

parseValues :: String -> Cuboid
parseValues input = (head splitValues, head . tail $ splitValues, last splitValues)
  where
    splitValues = map parseValue . splitOn "," $ input :: [CuboidValue]

parseValue :: String -> CuboidValue
parseValue input = (head splitValue, last splitValue)
  where
    splitValue = map read . splitOn ".." . last . splitOn "=" $ input :: [Int]

isOn :: EnabledGeometry -> Bool
isOn (isOn, _) = isOn

howManyCubes :: Cuboid -> Integer
howManyCubes cuboid = x * y * z
  where
    ((x1, x2), (y1, y2), (z1, z2)) = cuboid
    x = toInteger (x2 - x1 + 1)
    y = toInteger (y2 - y1 + 1) 
    z = toInteger (z2 - z1 + 1)

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection cuboidA cuboidB
  | x1 > x2 = Nothing
  | y1 > y2 = Nothing
  | z1 > z2 = Nothing
  | otherwise = Just ((x1, x2), (y1, y2), (z1, z2))
  where
    ((xA1, xA2), (yA1, yA2), (zA1, zA2)) = cuboidA
    ((xB1, xB2), (yB1, yB2), (zB1, zB2)) = cuboidB
    x1 = maximum [xA1, xB1]
    x2 = minimum [xA2, xB2]
    y1 = maximum [yA1, yB1]
    y2 = minimum [yA2, yB2]
    z1 = maximum [zA1, zB1]
    z2 = minimum [zA2, zB2]

geometryIntersection :: Geometry -> Geometry -> Maybe Geometry
geometryIntersection (Simple cuboidA) (Simple cuboidB) = fmap Simple (intersection cuboidA cuboidB)
geometryIntersection (Simple cuboidA) (Without cuboidB withoutB)
  | null withoutC = fmap Simple cuboidC
  | otherwise = fmap (`Without` withoutC) cuboidC
  where
    geometryA = Simple cuboidA
    cuboidC = intersection cuboidA cuboidB
    withoutC = mapMaybe (`geometryIntersection` geometryA) withoutB
geometryIntersection (Without cuboidA withoutA) (Simple cuboidB) = geometryIntersection (Simple cuboidB) (Without cuboidA withoutA)
geometryIntersection (Without cuboidA withoutA) (Without cuboidB withoutB)
  | null withoutC = fmap Simple cuboidC
  | otherwise = fmap (`Without` withoutC) cuboidC
  where
    cuboidC = intersection cuboidA cuboidB
    geometryA = Simple cuboidA
    withoutBA = mapMaybe (`geometryIntersection` geometryA) withoutB
    geometryB = Simple cuboidB
    withoutAB = mapMaybe (`geometryIntersection` geometryB) withoutA
    withoutC = nub . (++ withoutAB) $ withoutBA

cuboidOf :: RebootStep -> Cuboid
cuboidOf (_, cuboid) = cuboid

onlyWithin :: Cuboid -> RebootStep -> Maybe RebootStep
onlyWithin withinCuboid (onOff, cuboid)
  | isNothing intersectionCuboid = Nothing
  | otherwise = Just (onOff, fromJust intersectionCuboid)
  where
    intersectionCuboid = intersection cuboid withinCuboid

without :: Geometry -> Geometry -> Maybe Geometry
without (Simple cuboidA) (Simple cuboidB)
  | cuboidA == cuboidB = Nothing
  | otherwise = Just . fromMaybe geometryA $ cuboidC
  where
    geometryA = Simple cuboidA
    cuboidC = (\a -> Without cuboidA [Simple a]) <$> intersection cuboidA cuboidB
without (Simple cuboidA) (Without cuboidB withoutB) = Just . fromMaybe geometryA $ geometryC
  where
    geometryA = Simple cuboidA
    geometryB = Without cuboidB withoutB
    geometryC = geometryIntersection geometryA geometryB
    withoutC = mapMaybe (geometryIntersection geometryA) withoutB
without (Without cuboidA withoutA) (Simple cuboidB)
  | geometryB `elem` withoutA = Just geometryA
  | cuboidA == cuboidB = Nothing
  | isNothing maybeIntersectionAB = Just geometryA
  | otherwise = Just (Without cuboidA withoutC)
  where
    geometryA = Without cuboidA withoutA
    geometryB = Simple cuboidB
    maybeIntersectionAB = intersection cuboidA cuboidB :: Maybe Cuboid
    intersectionAB = fromJust maybeIntersectionAB :: Cuboid
    withoutC = (++ [Simple intersectionAB]) . mapMaybe (\a -> without a . Simple $ intersectionAB) $ withoutA :: [Geometry]
without (Without cuboidA withoutA) (Without cuboidB withoutB)
  | geometryA == geometryB = Nothing
  | otherwise = (`Without` withoutC) <$> cuboidC
  where
    geometryA = Without cuboidA withoutA
    geometryB = Without cuboidB withoutB
    cuboidC = intersection cuboidA cuboidB
    withoutBIntersectionA = mapMaybe (geometryIntersection geometryA) withoutB :: [Geometry]
    withoutC = mapMaybe (`withoutAll` withoutBIntersectionA) withoutA

withoutAll :: Geometry -> Geometries -> Maybe Geometry
withoutAll geometry = foldl maybeWithout (Just geometry)
  where
    maybeWithout :: Maybe Geometry -> Geometry -> Maybe Geometry
    maybeWithout g o
      | isJust g = (`without` o) . fromJust $ g
      | otherwise = Nothing

collect :: EnabledGeometry -> Geometries -> Geometries
collect toCollect collected
  | isOn toCollect = collectOn
  | otherwise = collectOff
  where
    (_, geometry) = toCollect

    collectOn :: Geometries
    collectOn
      | geometry `elem` collected = collected
      | otherwise = newCollected
      where
        newCollected = (++ [geometry]) . mapMaybe (`without` geometry) $ collected

    collectOff :: Geometries
    collectOff = mapMaybe (`without` geometry) $ collected

count :: Geometry -> Integer
count (Simple cuboid) = howManyCubes cuboid
count (Without cuboid without) = cubes - noCubes
  where
    cubes = howManyCubes cuboid
    noCubes = sum . map count $ without

toGeometry :: RebootStep -> EnabledGeometry
toGeometry rebootStep = (isOn rebootStep, Simple cuboid)
  where
    (onOff, cuboid) = rebootStep

    isOn :: RebootStep -> Bool
    isOn (onOff, _)
      | onOff == "off" = False
      | onOff == "on" = True
      | otherwise = error "illegal onOff value"

howManyAreOn :: RebootSteps -> Integer
howManyAreOn rebootSteps = sum . map count $ collected
  where
    geometries = map toGeometry rebootSteps :: EnabledGeometries
    collected = foldl (flip collect) [] geometries :: Geometries

howManyCubesAreOn :: String -> Integer
howManyCubesAreOn input = howManyAreOn . mapMaybe (onlyWithin filterCuboid) $ steps
  where
    steps = parseInput input :: RebootSteps
    filterCuboid = ((-50, 50), (-50, 50), (-50, 50))

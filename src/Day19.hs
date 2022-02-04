module Day19 where

-- https://adventofcode.com/2021/day/19

import Data.List (nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Prelude hiding (subtract)
import Data.Set (Set, fromList, toList, empty, union, intersection)

parseInput :: String -> [[[Int]]]
parseInput = map parseScanner . splitOn [""] . lines
  where
    parseScanner :: [String] -> [[Int]]
    parseScanner = map (map read . splitOn ",") . tail . takeWhile (/= "")

type Vector = (Int, Int, Int)

type Vectors = Set Vector

type Matrix = (Vector, Vector, Vector)

type Scan = (Vector, [Rotation], Vectors)

type Rotation = (Int, Vector)

row :: Int -> Matrix -> Vector
row 1 (a, b, c) = a
row 2 (a, b, c) = b
row 3 (a, b, c) = c

column :: Int -> Matrix -> Vector
column 1 (a, b, c) = (a1, b1, c1)
  where
    (a1, a2, a3) = a
    (b1, b2, b3) = b
    (c1, c2, c3) = c
column 2 (a, b, c) = (a2, b2, c2)
  where
    (a1, a2, a3) = a
    (b1, b2, b3) = b
    (c1, c2, c3) = c
column 3 (a, b, c) = (a3, b3, c3)
  where
    (a1, a2, a3) = a
    (b1, b2, b3) = b
    (c1, c2, c3) = c

distance :: Vector -> Double
distance (x, y, z) = sqrt (x' + y' + z')
  where
    x' = (^ 2) . fromIntegral $ x :: Double
    y' = (^ 2) . fromIntegral $ y :: Double
    z' = (^ 2) . fromIntegral $ z :: Double

scalar :: Vector -> Vector -> Int
scalar (x1, x2, x3) (y1, y2, y3) = x1 * y1 + x2 * y2 + x3 * y3

vNegate :: Vector -> Vector
vNegate (x, y, z) = (negate x, negate y, negate z)

mNegate :: Matrix -> Matrix
mNegate (a, b, c) = (vNegate a, vNegate b, vNegate c)

mAdd :: Matrix -> Matrix -> Matrix
mAdd (a1, b1, c1) (a2, b2, c2) = (add a1 a2, add b1 b2, add c1 c2)

mMult :: Matrix -> Matrix -> Matrix
mMult mA mB =
  ( (scalar rowA1 columnB1, scalar rowA1 columnB2, scalar rowA1 columnB2),
    (scalar rowA2 columnB1, scalar rowA2 columnB2, scalar rowA2 columnB2),
    (scalar rowA3 columnB1, scalar rowA3 columnB2, scalar rowA3 columnB2)
  )
  where
    rowA1 = row 1 mA :: Vector
    rowA2 = row 2 mA :: Vector
    rowA3 = row 3 mA :: Vector
    columnB1 = column 1 mB :: Vector
    columnB2 = column 2 mB :: Vector
    columnB3 = column 3 mB :: Vector

mvMult :: Matrix -> Vector -> Vector
mvMult mA vector = (scalar rowA1 vector, scalar rowA2 vector, scalar rowA3 vector)
  where
    rowA1 = row 1 mA :: Vector
    rowA2 = row 2 mA :: Vector
    rowA3 = row 3 mA :: Vector

positionVector :: Vector
positionVector = (0, 0, 0)

neutralRotation :: Matrix
neutralRotation = ((1, 0, 0), (0, 1, 0), (0, 0, 1))

toVector :: [Int] -> Vector
toVector xs = (head xs, head . tail $ xs, last xs)

toVectors :: [[Int]] -> Vectors
toVectors = fromList . map toVector

toScan :: [[Int]] -> Scan
toScan xs = (positionVector, [], beacons)
  where
    beacons = toVectors xs :: Vectors

toScans :: [[[Int]]] -> [Scan]
toScans = map toScan

cos' :: Int -> Int
cos' x = round . cos $ (pi / 180.0 * fromIntegral x)

sin' :: Int -> Int
sin' x = round . sin $ (pi / 180.0 * fromIntegral x)

invert :: Rotation -> Rotation
invert (angle, axis) = (negate angle, axis)

toMatrix :: Rotation -> Matrix
toMatrix (angle, axis) =
  ( (cosAngle + x * x * (1 - cosAngle), x * y * (1 - cosAngle) - z * sinAngle, x * z * (1 - cosAngle) + y * sinAngle),
    (y * x * (1 - cosAngle) + z * sinAngle, cosAngle + y * y * (1 - cosAngle), y * z * (1 - cosAngle) - x * sinAngle),
    (z * x * (1 - cosAngle) - y * sinAngle, z * y * (1 - cosAngle) + x * sinAngle, cosAngle + z * z * (1 - cosAngle))
  )
  where
    cosAngle = cos' angle :: Int
    sinAngle = sin' angle :: Int
    (x, y, z) = axis

add :: Vector -> Vector -> Vector
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subtract :: Vector -> Vector -> Vector
subtract (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

rotate :: Rotation -> Vector -> Vector
rotate rotation vector = (`mvMult` vector) . toMatrix $ rotation

rotateAll :: Rotation -> Vectors -> Vectors
rotateAll rotation = fromList . map (mvMult . toMatrix $ rotation) . toList

rotateAllBy :: [Rotation] -> Vectors -> Vectors
rotateAllBy [] vectors = vectors
rotateAllBy rotations vectors = rotateAllBy (init rotations) . fromList . map (mvMult . toMatrix $ rotation) . toList $ vectors
  where
    rotation = last rotations

rotateScan :: Rotation -> Scan -> Scan
rotateScan rotationMatrix (position, rotations, beacons) =
  (position, rotations ++ [rotationMatrix], movedBeacons)
  where
    relativeBeacons = fromList . map (`subtract` position) . toList $ beacons :: Vectors
    rotatedBeacons = rotateAll rotationMatrix relativeBeacons :: Vectors
    movedBeacons = fromList . map (`add` position) . toList $ rotatedBeacons

rotateScanBy :: [Rotation] -> Scan -> Scan
rotateScanBy [] scan = scan
rotateScanBy rotations scan = foldl (flip rotateScan) scan rotations

revertRotations :: [Rotation] -> Scan -> Scan
revertRotations rotations scan = foldl (flip rotateScan) scan . map invert $ rotations

moveScan :: Vector -> Scan -> Scan
moveScan position scan = (scannerPosition `add` position, rotation, newBeacons)
  where
    (scannerPosition, rotation, beacons) = scan
    newBeacons = fromList . map (`add` position) . toList $ beacons

rotateScanX :: Scan -> Scan
rotateScanX = rotateScan rotation
  where
    rotation = (90, (1, 0, 0))

rotateScanY :: Scan -> Scan
rotateScanY = rotateScan rotation
  where
    rotation = (90, (0, 1, 0))

rotateScanZ :: Scan -> Scan
rotateScanZ = rotateScan rotation
  where
    rotation = (90, (0, 0, 1))

rotationsX :: Scan -> [Scan]
rotationsX = take 4 . iterate rotateScanX

rotationsY :: Scan -> [Scan]
rotationsY = take 4 . iterate rotateScanY

rotationsZ :: Scan -> [Scan]
rotationsZ = take 4 . iterate rotateScanZ

subtractAllBy :: Vector -> Scan -> Scan
subtractAllBy vector (position, rotation, beacons) = (subtract position vector, rotation, newBeacons)
  where
    newBeacons = fromList . map (subtract vector) . toList $ beacons

relativePositions :: Scan -> [Scan]
relativePositions scan = scanl (\_ beacon -> subtractAllBy beacon scan) scan . sortBy (flip (\v1 v2 -> compare (distance v1) (distance v2))) . toList $ beacons
  where
    (scannerPosition, _, beacons) = scan

allPositions :: Scan -> [Scan]
allPositions scan =
  concatMap relativePositions
    . concatMap rotationsZ
    . concatMap rotationsY
    . concatMap rotationsX
    $ [scan]

haveOverlapping :: Scan -> Scan -> Bool
haveOverlapping scan0 scan1 = (>= 12) . length $ overlapping
  where
    (_, _, beacons0) = scan0
    (_, _, beacons1) = scan1
    overlapping = (`intersection` beacons1) beacons0 :: Vectors

findOverlapping :: Scan -> Scan -> Maybe (Scan, Scan)
findOverlapping scan0 scan1
  | null overlappings = Nothing
  | otherwise = Just . head $ overlappings
  where
    allScans0 = relativePositions scan0 :: [Scan]
    allScans1 = allPositions scan1 :: [Scan]

    combinations = [(x, y) | x <- allScans0, y <- allScans1] :: [(Scan, Scan)]

    overlappings = filter (uncurry haveOverlapping) combinations

relativeScannerPositions :: Scan -> Scan -> Maybe Scan
relativeScannerPositions scan0 scan1
  | isNothing foundOverlapping = Nothing
  | otherwise = Just (position, [], beacons)
  where
    (scanner0Position, _, _) = scan0
    foundOverlapping = findOverlapping scan0 scan1 :: Maybe (Scan, Scan)

    ((pos1, rotations1, _), (pos2, rotations2, _)) = fromJust foundOverlapping

    relativeScanner1Position = pos1 `subtract` pos2
    absoluteScanner1Position = scanner0Position `subtract` relativeScanner1Position

    (position, _, beacons) = revertRotations rotations1 . rotateScanBy rotations2 . moveScan absoluteScanner1Position $ scan1

beaconsFrom :: Scan -> Vectors
beaconsFrom (_, _, beacons) = beacons

absoluteBeaconPositions :: [Scan] -> Vectors
absoluteBeaconPositions (scan0 : scans) = (scan0Beacons `union`) . search $ ([scan0], scans)
  where
    scan0Beacons = beaconsFrom scan0 :: Vectors

    search :: ([Scan], [Scan]) -> Vectors
    search (_, []) = empty
    search (scan0 : otherScans, open) = (foundBeacons `union`) $ search (otherScans ++ found, newOpen)
      where
        scannerPositions = map (\o -> (o, relativeScannerPositions scan0 o)) open :: [(Scan, Maybe Scan)]
        found = map (fromJust . snd) . filter (isJust . snd) $ scannerPositions :: [Scan]
        newOpen = map (fst) . filter (isNothing . snd) $ scannerPositions :: [Scan]
        foundBeacons = foldl union empty (map beaconsFrom found) :: Vectors

howManyBeacons :: String -> Int
howManyBeacons input = length . absoluteBeaconPositions $ scans
  where
    scans = toScans . parseInput $ input :: [Scan]

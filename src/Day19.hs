module Day19 where

-- https://adventofcode.com/2021/day/19

-- https://adventofcode.com/2021/day/19
import Data.List (nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace (traceShow)
import Prelude hiding (subtract)

parseInput :: String -> [[[Int]]]
parseInput = map parseScanner . splitOn [""] . lines
  where
    parseScanner :: [String] -> [[Int]]
    parseScanner = map (map read . splitOn ",") . tail . takeWhile (/= "")

type Vector = (Int, Int, Int)

type Vectors = [Vector]

type Matrix = (Vector, Vector, Vector)

type Scan = (Vector, [Matrix], Vectors)

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
toVectors = map toVector

toScan :: [[Int]] -> Scan
toScan xs = (positionVector, [], beacons)
  where
    beacons = toVectors xs :: Vectors

toScans :: [[[Int]]] -> [Scan]
toScans = map toScan

rotateX :: Vector -> Vector
rotateX = mvMult rotation
  where
    rotation = ((1, 0, 0), (0, 0, -1), (0, 1, 0))

rotateY :: Vector -> Vector
rotateY = mvMult rotation
  where
    rotation = ((0, 0, 1), (0, 1, 0), (-1, 0, 0))

rotateZ :: Vector -> Vector
rotateZ = mvMult rotation
  where
    rotation = ((0, -1, 0), (1, 0, 0), (0, 0, 1))

rotate :: Matrix -> Vector -> Vector
rotate = mvMult

rotateAll :: Matrix -> Vectors -> Vectors
rotateAll rotation = map (mvMult rotation)

rotateScan :: Matrix -> Scan -> Scan
rotateScan rotationMatrix (position, rotations, beacons) = (rotate rotationMatrix position, rotations ++ [rotationMatrix], rotateAll rotationMatrix beacons)

moveScan :: Vector -> Scan -> Scan
moveScan position scan = (scannerPosition `add` position, rotation, map (`add` position) beacons)
  where
    (scannerPosition, rotation, beacons) = scan

rotateScanX :: Scan -> Scan
rotateScanX = rotateScan rotation
  where
    rotation = ((1, 0, 0), (0, 0, -1), (0, 1, 0))

rotateScanY :: Scan -> Scan
rotateScanY = rotateScan rotation
  where
    rotation = ((0, -1, 0), (1, 0, 0), (0, 0, 1))

rotateScanZ :: Scan -> Scan
rotateScanZ = rotateScan rotation
  where
    rotation = ((0, 0, 1), (0, 1, 0), (-1, 0, 0))

rotationsX :: Scan -> [Scan]
rotationsX = take 4 . iterate rotateScanX

rotationsY :: Scan -> [Scan]
rotationsY = take 4 . iterate rotateScanY

rotationsZ :: Scan -> [Scan]
rotationsZ = take 4 . iterate rotateScanZ

add :: Vector -> Vector -> Vector
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subtract :: Vector -> Vector -> Vector
subtract (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

-- subtractAllBy :: Vector -> Vectors -> Vectors
-- subtractAllBy vector = map (subtract vector)

subtractAllBy :: Vector -> Scan -> Scan
subtractAllBy vector (position, rotation, beacons) = (subtract position vector, rotation, map (subtract vector) beacons)

relativePositions :: Scan -> [Scan]
relativePositions scan = scanl (\_ beacon -> subtractAllBy beacon scan) scan . sortBy (flip (\v1 v2 -> compare (distance v1) (distance v2))) $ beacons
  where
    (scannerPosition, _, beacons) = scan

relativeToScanner :: Scan -> Scan
relativeToScanner scan
  | scannerPosition == (0, 0, 0) = scan
  | otherwise = ((0, 0, 0), rotation, map (scannerPosition `subtract`) beacons)
  where
    (scannerPosition, rotation, beacons) = scan

allPositions :: Scan -> [Scan]
allPositions scan = nub . concatMap rotationsZ . nub . concatMap rotationsY . nub . concatMap rotationsX . concatMap relativePositions $ [scan]

haveOverlapping :: Scan -> Scan -> Bool
haveOverlapping scan0 scan1 = (>= 12) . length . filter (`elem` beacons1) $ beacons0
  where
    (_, _, beacons0) = scan0
    (_, _, beacons1) = scan1

findOverlapping :: Scan -> Scan -> Maybe (Scan, Scan)
findOverlapping scan0 scan1
  | null overlappings = Nothing
  | otherwise = Just . head $ overlappings
  where
    allScans0 = relativePositions $ scan0 :: [Scan]
    allScans1 = allPositions $ scan1 :: [Scan]

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

    (position, _, beacons) = moveScan absoluteScanner1Position . revertRotations (rotations1 ++ rotations2) $ scan1

revertRotations :: [Matrix] -> Scan -> Scan
revertRotations rotations scan = foldl (flip rotateScan) scan . map mNegate $ rotations
  where
    bla = map mNegate $ rotations

beaconsFrom :: Scan -> Vectors
beaconsFrom (_, _, beacons) = beacons

howManyBeacons :: String -> Int
howManyBeacons input = length . absoluteBeaconPositions $ scans
  where
    scans = toScans . parseInput $ input :: [Scan]
    combinations = [(x, y) | x <- scans, y <- scans, x /= y] :: [(Scan, Scan)]

absoluteBeaconPositions :: [Scan] -> Vectors
absoluteBeaconPositions (first:scans) = search ([first], scans)
  where
    search :: ([Scan], [Scan]) -> Vectors
    search (_, []) = []
    search (f:found, open) = nub . ((concatMap beaconsFrom $ newFound )++) $ (search (found ++ newFound, newOpen))
      where
        scannerPositions = map (\o -> (o, relativeScannerPositions f o))$ open :: [(Scan, Maybe Scan)]
        newFound = map (fromJust . snd) . filter ( isJust . snd) $ scannerPositions :: [Scan]
        newOpen = map (fst) . filter (isNothing . snd) $ scannerPositions :: [Scan]

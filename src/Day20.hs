module Day20 where

-- https://adventofcode.com/2021/day/20

import Data.Bits (Bits (clearBit, setBit, testBit), (.&.), (.|.))
import Data.List.Split (splitOn)
import Data.Map (Map, empty, fromList, insert, keys, lookup, member, toList)
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

parseInput :: String -> ([Bool], [[Bool]])
parseInput input = (imageEnhancementAlgorithm, inputImage)
  where
    splitInput = splitOn [""] . lines $ input :: [[String]]
    imageEnhancementAlgorithm = map toBit . head . head $ splitInput
    inputImage = map (map toBit) . last $ splitInput

    toBit :: Char -> Bool
    toBit '.' = False
    toBit '#' = True
    toBit c = error ("Cannot convert to bit: " ++ [c])

type ImageEnhancementAlgorithm = Map Int Bool

imageEnhancementAlgorithmFromBits :: [Bool] -> ImageEnhancementAlgorithm
imageEnhancementAlgorithmFromBits = fromList . zip [0 ..]

enhanceValue :: ImageEnhancementAlgorithm -> Int -> Bool
enhanceValue alg value = fromJust . lookup value $ alg

type Position = (Int, Int)

positionsFromSize :: Size -> [Position]
positionsFromSize size = [(x, y) | x <- [0 .. (sizeX - 1)], y <- [0 .. (sizeY - 1)]]
  where
    (sizeX, sizeY) = size

type Size = (Int, Int)

sizeFromBits :: [[Bool]] -> Size
sizeFromBits bits = (length . head $ bits, length bits)

type BitMap = Map Position Int

isLit :: Int -> Bool
isLit = (`testBit` 4)

type Image = (Size, BitMap)

emptyImage :: Size -> Image
emptyImage size = (size, bitmap)
  where
    positions = positionsFromSize size :: [Position]
    bitmap = foldl (\m p -> insert p 0 m) empty positions

setPixel :: Position -> Bool -> Image -> Image
setPixel position isLit image = (size, newBitmap)
  where
    (size, bitmap) = image
    old = fromMaybe 0 . lookup position $ bitmap :: Int
    newBitmap =
      setCenter position isLit
        . setNorth position isLit
        . setSouth position isLit
        . setWest position isLit
        . setEast position isLit
        . setNorthWest position isLit
        . setNorthEast position isLit
        . setSouthEast position isLit
        . setSouthWest position isLit
        $ bitmap ::
        BitMap

updatePixel :: Position -> Bool -> Image -> Image
updatePixel position lit image
  | isLit old == lit = image
  | otherwise = setPixel position lit image
  where
    (size, bitmap) = image
    old = fromMaybe 0 . lookup position $ bitmap :: Int

setCenter :: Position -> Bool -> BitMap -> BitMap
setCenter position isLit bitmap = insert position new bitmap
  where
    old = fromMaybe 0 . lookup position $ bitmap :: Int
    new
      | isLit = setBit old 4
      | otherwise = clearBit old 4

setNorth :: Position -> Bool -> BitMap -> BitMap
setNorth position isLit bitmap
  | member north bitmap = insert north new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    north = (x, y - 1) :: Position
    old = fromMaybe 0 . lookup north $ bitmap :: Int
    new
      | isLit = setBit old 1
      | otherwise = clearBit old 1

setSouth :: Position -> Bool -> BitMap -> BitMap
setSouth position isLit bitmap
  | member south bitmap = insert south new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    south = (x, y + 1) :: Position
    old = fromMaybe 0 . lookup south $ bitmap :: Int
    new
      | isLit = setBit old 7
      | otherwise = clearBit old 7

setWest :: Position -> Bool -> BitMap -> BitMap
setWest position isLit bitmap
  | member west bitmap = insert west new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    west = (x - 1, y) :: Position
    old = fromMaybe 0 . lookup west $ bitmap :: Int
    new
      | isLit = setBit old 3
      | otherwise = clearBit old 3

setEast :: Position -> Bool -> BitMap -> BitMap
setEast position isLit bitmap
  | member east bitmap = insert east new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    east = (x + 1, y) :: Position
    old = fromMaybe 0 . lookup east $ bitmap :: Int
    new
      | isLit = setBit old 5
      | otherwise = clearBit old 5

setNorthWest :: Position -> Bool -> BitMap -> BitMap
setNorthWest position isLit bitmap
  | member northWest bitmap = insert northWest new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    northWest = (x - 1, y - 1) :: Position
    old = fromMaybe 0 . lookup northWest $ bitmap :: Int
    new
      | isLit = setBit old 0
      | otherwise = clearBit old 0

setNorthEast :: Position -> Bool -> BitMap -> BitMap
setNorthEast position isLit bitmap
  | member northEast bitmap = insert northEast new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    northEast = (x + 1, y - 1) :: Position
    old = fromMaybe 0 . lookup northEast $ bitmap :: Int
    new
      | isLit = setBit old 2
      | otherwise = clearBit old 2

setSouthEast :: Position -> Bool -> BitMap -> BitMap
setSouthEast position isLit bitmap
  | member southEast bitmap = insert southEast new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    southEast = (x + 1, y + 1) :: Position
    old = fromMaybe 0 . lookup southEast $ bitmap :: Int
    new
      | isLit = setBit old 8
      | otherwise = clearBit old 8

setSouthWest :: Position -> Bool -> BitMap -> BitMap
setSouthWest position isLit bitmap
  | member southWest bitmap = insert southWest new bitmap
  | otherwise = bitmap
  where
    (x, y) = position
    southWest = (x - 1, y + 1) :: Position
    old = fromMaybe 0 . lookup southWest $ bitmap :: Int
    new
      | isLit = setBit old 6
      | otherwise = clearBit old 6

imageFromBits :: [[Bool]] -> Image
imageFromBits bits = newImage
  where
    size = (length $ bits, length . head $ bits) :: Size
    newImage = foldl (\image (p, b) -> setPixel p b image) (emptyImage size) . concatMap row . zip [0..] $ bits

    row :: (Int, [Bool]) -> [(Position, Bool)]
    row (y, rowBits) = zipWith (\ x b -> ((x, y), b)) [0..] rowBits

extendImage :: Image -> Image
extendImage image = foldl transferPixel (emptyImage newSize) . toList $ bitmap
  where
    (size, bitmap) = image
    (sizeX, sizeY) = size

    newSize = (sizeX + 2, sizeY + 2)

    transferPixel :: Image -> (Position, Int) -> Image
    transferPixel image (position, i) = updatePixel newPosition (isLit i) image
      where
        (x, y) = position
        newPosition = (x + 1, y + 1) :: Position

enhanceImage :: ImageEnhancementAlgorithm -> Image -> Image
enhanceImage alg image = foldl enhancePixel extended . toList $ extendedBitmap
  where
    extended = extendImage image :: Image
    (_, extendedBitmap) = extended

    enhancePixel :: Image -> (Position, Int) -> Image
    enhancePixel image (position, i) = updatePixel position (enhanceValue alg i) image

countLit :: Image -> Int
countLit image = length . filter (== True) . map (isLit . snd) . toList $ bitmap
  where
    (_, bitmap) = image

howManyPixelsAreLit :: String -> Int
howManyPixelsAreLit input = countLit . enhanceImage alg . enhanceImage alg $ inputImage
  where
    (algBits, imageBits) = parseInput input
    alg = imageEnhancementAlgorithmFromBits algBits :: ImageEnhancementAlgorithm
    inputImage = imageFromBits imageBits :: Image

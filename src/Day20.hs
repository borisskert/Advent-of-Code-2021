{-# LANGUAGE TupleSections #-}

module Day20 where

import qualified Control.Arrow as Data.Bifunctor
import Data.Bits (Bits (clearBit, setBit, testBit))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map, elems, empty, fromList, insert, keys, lookup, toList)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Prelude hiding (lookup)

parseInput :: String -> (String, String)
parseInput = (\[a, b] -> (a, b)) . splitOn "\n\n"

type Light = Int

type IsLit = Bool

type EnhancementAlgorithm = Map Light IsLit

charToBit :: Char -> IsLit
charToBit = (== '#')

parseEnhancementAlgorithm :: String -> EnhancementAlgorithm
parseEnhancementAlgorithm input = fromList . zip [0 ..] . map charToBit $ input
  where
    mode = charToBit . head $ input :: Bool

type Position = (Int, Int)

type RawImage = Map Position IsLit

type Image = Map Position Light

type Size = (Int, Int)

parseRawImage :: String -> RawImage
parseRawImage input = bitmap
  where
    inputLines = lines input :: [String]

    bitLines = map (map charToBit) inputLines :: [[IsLit]]

    bitmap =
      fromList
        . concatMap (\(y, bitsLine) -> zipWith (\x bit -> ((x, y), bit)) [0 ..] bitsLine)
        . zip [0 ..]
        $ bitLines ::
        RawImage

toString :: Image -> String
toString image =
  intercalate "\n"
    . map (\y -> map (toChar . isLit . (\p -> fromMaybe 0 . lookup p $ image) . (,y)) xs)
    $ ys
  where
    positions = keys image :: [Position]
    minX = minimum . map fst $ positions
    maxX = maximum . map fst $ positions
    minY = minimum . map snd $ positions
    maxY = maximum . map snd $ positions
    xs = [minX, (minX + 1) .. maxX]
    ys = [minY, (minY + 1) .. maxY]
    allPositions = [(x, y) | x <- xs, y <- ys]

    toChar :: Bool -> Char
    toChar True = '#'
    toChar False = '.'

setBitAt :: Light -> IsLit -> Int -> Light
setBitAt light isLit index
  | isLit = setBit light index
  | otherwise = clearBit light index

insertBitAt :: Position -> IsLit -> Int -> Image -> Image
insertBitAt position bit index image = newIntmap
  where
    light = fromMaybe 0 . lookup position $ image :: Light
    newLight = setBitAt light bit index :: Light
    newIntmap = insert position newLight image

insertBitAtCenter :: Position -> IsLit -> Image -> Image
insertBitAtCenter position bit = insertBitAt position bit 4

insertBitAtNorthWest :: Position -> IsLit -> Image -> Image
insertBitAtNorthWest position bit = insertBitAt neigbor bit 0
  where
    (x, y) = position
    neigbor = (x -1, y -1) :: Position

insertBitAtNorth :: Position -> IsLit -> Image -> Image
insertBitAtNorth position bit = insertBitAt neigbor bit 1
  where
    (x, y) = position
    neigbor = (x, y -1) :: Position

insertBitAtNorthEast :: Position -> IsLit -> Image -> Image
insertBitAtNorthEast position bit = insertBitAt neigbor bit 2
  where
    (x, y) = position
    neigbor = (x + 1, y -1) :: Position

insertBitAtEast :: Position -> IsLit -> Image -> Image
insertBitAtEast position bit = insertBitAt neigbor bit 5
  where
    (x, y) = position
    neigbor = (x + 1, y) :: Position

insertBitAtSouthEast :: Position -> IsLit -> Image -> Image
insertBitAtSouthEast position bit = insertBitAt neigbor bit 8
  where
    (x, y) = position
    neigbor = (x + 1, y + 1) :: Position

insertBitAtSouth :: Position -> IsLit -> Image -> Image
insertBitAtSouth position bit = insertBitAt neigbor bit 7
  where
    (x, y) = position
    neigbor = (x, y + 1) :: Position

insertBitAtSouthWest :: Position -> IsLit -> Image -> Image
insertBitAtSouthWest position bit = insertBitAt neigbor bit 6
  where
    (x, y) = position
    neigbor = (x - 1, y + 1) :: Position

insertBitAtWest :: Position -> IsLit -> Image -> Image
insertBitAtWest position bit = insertBitAt neigbor bit 3
  where
    (x, y) = position
    neigbor = (x - 1, y) :: Position

insertBit :: Position -> IsLit -> Image -> Image
insertBit position bit image = newImage
  where
    newImage =
      insertBitAtWest position bit
        . insertBitAtSouthWest position bit
        . insertBitAtSouth position bit
        . insertBitAtSouthEast position bit
        . insertBitAtEast position bit
        . insertBitAtNorthEast position bit
        . insertBitAtNorth position bit
        . insertBitAtNorthWest position bit
        . insertBitAtCenter position bit
        $ image

lightOnAt :: Position -> Image -> Image
lightOnAt position = insertBit position True

lightOffAt :: Position -> Image -> Image
lightOffAt position = insertBit position False

fromRawImage :: RawImage -> Image
fromRawImage bitmap = foldl (\image (position, bit) -> lightOnAt position image) empty . filter snd . toList $ bitmap
  where
    positions = keys bitmap :: [Position]

isLit :: Light -> IsLit
isLit light = testBit light 4

enhanceLight :: EnhancementAlgorithm -> Light -> IsLit
enhanceLight alg light = fromJust . lookup light $ alg

allPositionsExtendedBy :: Int -> Image -> [Position]
allPositionsExtendedBy extend image = [(x, y) | x <- xs, y <- ys]
  where
    positions = keys image :: [Position]
    minX = minimum . map fst $ positions
    maxX = maximum . map fst $ positions
    minY = minimum . map snd $ positions
    maxY = maximum . map snd $ positions
    xs = [(minX - extend) .. (maxX + extend)]
    ys = [(minY - extend) .. (maxY + extend)]

enhancePixel :: EnhancementAlgorithm -> (Position, Light) -> (Position, IsLit)
enhancePixel alg (position, light) = (position, isLit)
  where
    isLit = fromJust . lookup light $ alg

enhance :: EnhancementAlgorithm -> Image -> Image
enhance alg image = fromRawImage . fromList . toList $ newBitmap
  where
    newBitmap = fromList . map (enhancePixel alg) $ extended :: RawImage
    extended = map (\p -> (p, fromMaybe 0 . lookup p $ image)) . allPositionsExtendedBy 20 $ image

crop :: Size -> Image -> Image
crop (width, height) image = fromList . filter ((\(x, y) -> x >= newMinX && y >= newMinY && x <= newMaxX && y <= newMaxY) . fst) . toList $ image
  where
    positions = keys image :: [Position]
    minX = minimum . map fst $ positions
    maxX = maximum . map fst $ positions
    minY = minimum . map snd $ positions
    maxY = maximum . map snd $ positions

    newMinX = ((maxX - minX) - width) `div` 2 + minX
    newMaxX = newMinX + width
    newMinY = ((maxY - minY) - height) `div` 2 + minY
    newMaxY = newMinY + height

sizeOf :: Image -> Size
sizeOf image = (maxX - minX, maxY - minY)
  where
    positions = keys image :: [Position]
    minX = minimum . map fst $ positions
    maxX = maximum . map fst $ positions
    minY = minimum . map snd $ positions
    maxY = maximum . map snd $ positions

countLit :: Image -> Int
countLit = length . filter isLit . elems

howManyPixelsAreLit :: String -> Int
howManyPixelsAreLit input = countLit . crop (width + 2, height + 2) . enhance alg . enhance alg $ image
  where
    (algAsString, rawImageAsString) = parseInput input
    alg = parseEnhancementAlgorithm algAsString :: EnhancementAlgorithm
    rawImage = parseRawImage rawImageAsString :: RawImage
    image = fromRawImage rawImage :: Image
    (width, height) = sizeOf image

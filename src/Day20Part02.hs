module Day20Part02 where

-- https://adventofcode.com/2021/day/20#part2

import Data.Map (fromList, lookup, toList)
import Data.Maybe (fromMaybe)
import Day20 (EnhancementAlgorithm, Image, RawImage, allPositionsExtendedBy, countLit, crop, enhance, fromRawImage, parseEnhancementAlgorithm, parseInput, parseRawImage, sizeOf)
import Prelude hiding (lookup)

extendBy :: Int -> Image -> Image
extendBy pixels image =
  fromList
    . map (\p -> (p, fromMaybe 0 . lookup p $ image))
    . allPositionsExtendedBy pixels
    $ image

enhanceImageOnce :: EnhancementAlgorithm -> Image -> Image
enhanceImageOnce alg image = crop newSize . enhance alg $ image
  where
    (width, height) = sizeOf image
    newSize = (width + 2, height + 2)

enhanceImageTwice :: EnhancementAlgorithm -> Image -> Image
enhanceImageTwice alg image = crop newSize . enhance alg . enhance alg $ image
  where
    (width, height) = sizeOf image
    newSize = (width + 4, height + 4)

enhanceImage :: EnhancementAlgorithm -> Int -> Image -> Image
enhanceImage alg times image = enhanceImageRecursively alg times image
  where
    enhanceImageRecursively :: EnhancementAlgorithm -> Int -> Image -> Image
    enhanceImageRecursively _ 0 image = image
    enhanceImageRecursively alg 1 image = enhanceImageOnce alg image
    enhanceImageRecursively alg times image = enhanceImageRecursively alg (times - 2) nextImage
      where
        nextImage = enhanceImageTwice alg image :: Image

howManyPixelsAreLitExtended :: String -> Int -> Int
howManyPixelsAreLitExtended input times = countLit . enhanceImage alg times $ image
  where
    (algAsString, rawImageAsString) = parseInput input
    alg = parseEnhancementAlgorithm algAsString :: EnhancementAlgorithm
    rawImage = parseRawImage rawImageAsString :: RawImage
    image = fromRawImage rawImage :: Image
    (width, height) = sizeOf image
    newSize = (width + times * 2, height + times * 2)

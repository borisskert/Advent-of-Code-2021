module Day08 where

import Data.List (sort)
import Data.List.Split (splitOn)

-- https://adventofcode.com/2021/day/8

parseInput :: String -> [([String], [String])]
parseInput =
  map
    ( (\x -> (head x, last x))
        . map words
        . splitOn " | "
    )
    . lines

data Wireing = Wireing
  { a :: Char,
    b :: Char,
    c :: Char,
    d :: Char,
    e :: Char,
    f :: Char,
    g :: Char
  }
  deriving (Show, Eq)

determine :: [String] -> Wireing
determine signal = Wireing {a = a, b = b, c = c, d = d, e = e, f = f, g = g}
  where
    one = head . filter ((== 2) . length) $ signal :: String
    seven = head . filter ((== 3) . length) $ signal :: String
    four = head . filter ((== 4) . length) $ signal :: String

    a = head . filter (`notElem` one) $ seven :: Char
    three = head . filter (\x -> head one `elem` x && last one `elem` x && a `elem` x) . filter ((== 5) . length) $ signal :: String

    b = head . filter (\x -> x `notElem` three && x `notElem` one) $ four :: Char
    d = head . filter (\x -> x /= a && x `notElem` one && x `elem` four) $ three :: Char

    twos = filter (\x -> a `elem` x && d `elem` x && b `notElem` x && ((== 1) . length . filter (`elem` x) $ one)) . filter ((== 5) . length) $ signal :: [String]
    two = head twos :: String
    fives = filter (\x -> a `elem` x && b `elem` x && d `elem` x && ((== 1) . length . filter (`elem` x) $ one)) . filter ((== 5) . length) $ signal :: [String]
    five = head fives :: String

    f :: Char
    f
      | null twos = head . filter (`notElem` five) $ one
      | otherwise = head . filter (`notElem` two) $ one

    c = head . filter (/= f) $ one :: Char

    nine = head . filter (\x -> a `elem` x && b `elem` x && c `elem` x && d `elem` x && f `elem` x) . filter ((== 6) . length) $ signal :: String
    g = head . filter (\x -> x /= a && x /= b && x /= c && x /= d && x /= f) $ nine :: Char
    eight = head . filter ((== 7) . length) $ signal :: String
    e = head . filter (\x -> x /= a && x /= b && x /= c && x /= d && x /= f && x /= g) $ eight :: Char

toDigit :: Wireing -> String -> Int
toDigit wireing = readDigit . sort . map dewire
  where
    dewire :: Char -> Char
    dewire x
      | x == a wireing = 'a'
      | x == b wireing = 'b'
      | x == c wireing = 'c'
      | x == d wireing = 'd'
      | x == e wireing = 'e'
      | x == f wireing = 'f'
      | x == g wireing = 'g'
      | otherwise = error ("Unknown signal: " ++ [x])

    readDigit :: String -> Int
    readDigit "abcefg" = 0
    readDigit "cf" = 1
    readDigit "acdeg" = 2
    readDigit "acdfg" = 3
    readDigit "bcdf" = 4
    readDigit "abdfg" = 5
    readDigit "abdefg" = 6
    readDigit "acf" = 7
    readDigit "abcdefg" = 8
    readDigit "abcdfg" = 9

toDigits :: String -> [[Int]]
toDigits input = map go parsed
  where
    parsed = parseInput $ input :: [([String], [String])]

    go :: ([String], [String]) -> [Int]
    go (pattern, numbers) = map (toDigit wireing) numbers
      where
        wireing = determine pattern :: Wireing

digitsAppear :: [Int] -> String -> Int
digitsAppear digits = length . filter (`elem` digits) . concat . toDigits

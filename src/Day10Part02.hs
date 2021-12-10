module Day10Part02 where

-- https://adventofcode.com/2021/day/10#part2

import Data.List (sort)
import Data.Maybe (isNothing)
import Day10 (checkForError, parseInput)

toScore :: String -> Int
toScore = foldl (\total s -> total * 5 + s) 0 . map score
  where
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4

autocomplete :: String -> String
autocomplete = autoCompleteWithStack []
  where
    autoCompleteWithStack :: [Char] -> String -> String
    autoCompleteWithStack [] [] = []
    autoCompleteWithStack stack [] = stack
    autoCompleteWithStack stack (c : cs)
      | c `elem` closing = checkClosing stack (c : cs)
      | otherwise = autoCompleteWithStack (toClosed c : stack) cs
      where
        toClosed :: Char -> Char
        toClosed '(' = ')'
        toClosed '[' = ']'
        toClosed '{' = '}'
        toClosed '<' = '>'

        checkClosing :: [Char] -> String -> String
        checkClosing [] [] = []
        checkClosing [] (c : _) = error "got: " ++ [c]
        checkClosing (x : xs) (c : cs)
          | c == x = autoCompleteWithStack xs cs
          | otherwise = error "got: " ++ [c]

        closing = ")}]>"

middleScore :: String -> Int
middleScore input = (!! half) . sort $ scores
  where
    scores =
      map (toScore . autocomplete)
        . filter (isNothing . checkForError)
        . parseInput
        $ input ::
        [Int]
    half = (`div` 2) . length $ scores :: Int

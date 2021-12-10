module Day10 where

-- https://adventofcode.com/2021/day/10

import Data.Maybe (fromJust, isJust)

parseInput :: String -> [String]
parseInput = lines

checkForError :: String -> Maybe Char
checkForError = check []
  where
    check :: [Char] -> String -> Maybe Char
    check _ [] = Nothing
    check stack (c : cs)
      | c `elem` closing = checkClosing stack (c : cs)
      | otherwise = check (toClosed c : stack) cs
      where
        toClosed :: Char -> Char
        toClosed '(' = ')'
        toClosed '[' = ']'
        toClosed '{' = '}'
        toClosed '<' = '>'

        checkClosing :: [Char] -> String -> Maybe Char
        checkClosing [] [] = Nothing
        checkClosing [] (c : _) = Just c
        checkClosing (x : xs) (c : cs)
          | c == x = check xs cs
          | otherwise = Just c

        closing = ")}]>"

toScore :: Char -> Int
toScore ')' = 3
toScore ']' = 57
toScore '}' = 1197
toScore '>' = 25137

syntaxErrorScore :: String -> Int
syntaxErrorScore =
  sum
    . map (toScore . fromJust)
    . filter isJust
    . map checkForError
    . parseInput

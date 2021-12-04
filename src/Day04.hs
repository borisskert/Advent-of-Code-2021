module Day04 where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust)
import Debug.Trace (traceShow)

data Board = Board ([[Int]], [Int]) deriving (Show, Eq)

readBoardFrom :: String -> Board
readBoardFrom input = Board (board, [])
  where
    rows = lines input :: [String]
    table = map words rows :: [[String]]
    board = map (map read) table :: [[Int]]

readBoardsFrom :: String -> [Board]
readBoardsFrom = map readBoardFrom . splitOn "\n\n"

parseInput :: String -> ([Int], [Board])
parseInput input = (numbers, boards)
  where
    numbers = map read . splitOn "," . takeWhile (/= '\n') $ input :: [Int]
    boards = readBoardsFrom . drop 2 . dropWhile (/= '\n') $ input :: [Board]

addNumber :: Board -> Int -> Board
addNumber (Board (board, numbers)) number = Board (board, numbers ++ [number])

bingo :: Board -> Maybe [Int]
bingo board
  | null winners = Nothing
  | otherwise = Just . head $ winners
  where
    winnerHorizontal = bingoHorizontal board
    winnerVertical = bingoVertical board
    winners = catMaybes [winnerHorizontal, winnerVertical]

bingoHorizontal :: Board -> Maybe [Int]
bingoHorizontal (Board (board, numbers))
  | null winning = Nothing
  | otherwise = Just . head $ winning
  where
    winning = filter ((== 5) . length . filter (`elem` numbers)) board :: [[Int]]

bingoVertical :: Board -> Maybe [Int]
bingoVertical (Board (board, numbers))
  | null winning = Nothing
  | otherwise = Just . head $ winning
  where
    columnAt :: Int -> [Int]
    columnAt index = map (!! index) board

    winning = filter ((== 5) . length . filter (`elem` numbers)) . map columnAt $ [0 .. 4]

unmarked :: Board -> [Int]
unmarked (Board (board, numbers)) = filter (`notElem` numbers) . concat $ board

lastNumber :: Board -> Int
lastNumber (Board (_, numbers)) = last numbers

playSubmarineBingo :: String -> (Int, Int)
playSubmarineBingo input = (sum . unmarked $bingoBoard, lastNumber bingoBoard)
  where
    (numbers, boards) = parseInput input

    addNumberToBoards :: [Board] -> Int -> [Board]
    addNumberToBoards boards number = map (`addNumber` number) boards

    bingoBoard =
      head
        . filter (isJust . bingo)
        . concat
        . dropWhile (any ((/= Nothing) . bingo))
        . scanl addNumberToBoards boards
        $ numbers ::
        Board

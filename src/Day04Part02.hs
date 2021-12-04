module Day04Part02 where

import Data.List (maximumBy, sort, sortBy)
import Data.Ord (comparing)
import Day04

addNumbersTillBingo :: Board -> [Int] -> Board
addNumbersTillBingo board =
  head
    . dropWhile ((== Nothing) . bingo)
    . scanl addNumber board

byAddedNumbers :: Board -> Board -> Ordering
byAddedNumbers = comparing (\(Board (_, numbers)) -> length numbers)

letSquidWin :: String -> (Int, Int)
letSquidWin input = (sum . unmarked $ losingBoard, lastNumber losingBoard)
  where
    (numbers, boards) = parseInput input
    losingBoard =
      maximumBy byAddedNumbers
        . map (`addNumbersTillBingo` numbers)
        $ boards ::
        Board

module Day21 where

-- https://adventofcode.com/2021/day/21

import Data.List (sortOn)
import Data.List.Split (divvy)

type Space = Int

type Score = Int

type Scores = [Score]

type Round = Int

type Player = (String, Score, Space, Round)

type Dice = [Score]

type Game = [Player]

parseInput :: String -> [(String, Space)]
parseInput = map parsePlayerInput . lines

parsePlayerInput :: String -> (String, Space)
parsePlayerInput input = (name, score)
  where
    inputWords = words input
    score = read . last $ inputWords :: Int
    name = unwords . take 2 $ inputWords

dice :: Dice
dice = cycle [1 .. 100] :: Dice

newPlayer :: (String, Space) -> Player
newPlayer (name, space) = (name, 0, space, 0)

createGame :: [Player] -> Game
createGame players = players

turnOf :: Player -> Round
turnOf (_, _, _, turn) = turn

currentPlayer :: [Player] -> Player
currentPlayer = head . sortOn turnOf

turnPlayer :: [Score] -> Player -> Player
turnPlayer dieScores player = (name, newScore, newSpace, turn + 1)
  where
    (name, score, space, turn) = player
    dieScore = sum dieScores :: Int
    newSpace = (+ 1) . (`mod` 10) . (+ dieScore) . (+ negate 1) $ space
    newScore = score + newSpace

is :: Player -> Player -> Bool
is (name1, _, _, _) (name2, _, _, _) = name1 == name2

turn :: [Score] -> Game -> Game
turn dieScores game = nextPlayers
  where
    players = game
    currPlayer = currentPlayer players :: Player
    nextPlayers = map nextPlayer players

    nextPlayer :: Player -> Player
    nextPlayer player
      | player == currPlayer = turnPlayer dieScores player
      | otherwise = player

hasWinner :: Game -> Bool
hasWinner = any hasWon

hasWon :: Player -> Bool
hasWon (_, score, _, _) = score >= 1000

loserFrom :: Game -> Player
loserFrom = head . filter (not . hasWon)

toRound :: Player -> Round
toRound (_, _, _, round) = round

losingSituation :: String -> (Score, Round)
losingSituation input = (score, round)
  where
    players = map newPlayer . parseInput $ input :: [Player]
    newGame = createGame players :: Game
    game = head . filter hasWinner . scanl (flip turn) newGame $ dieScores
    dieTimes = 3
    (_, score, _, _) = loserFrom game :: Player
    dieScores = divvy dieTimes dieTimes dice :: [Scores]
    round = (* dieTimes) . sum . map toRound $ game :: Int

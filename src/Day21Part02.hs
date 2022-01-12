{-# LANGUAGE TupleSections #-}

module Day21Part02 where

-- https://adventofcode.com/2021/day/21#part2

import Data.Map (Map, delete, fromList, insert, keys, lookup, member)
import Data.Maybe
import Day21 (Score, Space, parseInput)
import Prelude hiding (lookup)
import Data.List (nub)

type Player = (String, Score, Space)

type Game = [Player]

type Games = Map Game Integer

type PlayerWins = (Integer, Integer)

type Scores = [(Score, Integer)]

hasWinner :: Game -> Bool
hasWinner = any hasWon

hasWon :: Player -> Bool
hasWon (_, score, _) = score >= 21

turnPlayer :: Score -> Player -> Player
turnPlayer dieScore player = (name, newScore, newSpace)
  where
    (name, score, space) = player
    newSpace = (+ 1) . (`mod` 10) . (+ dieScore) . (+ negate 1) $ space
    newScore = score + newSpace

turn :: Score -> Game -> Game
turn dieScore game = nextPlayers
  where
    players = game
    currPlayer = head players :: Player
    nextPlayers = reverse . map nextPlayer $ players

    nextPlayer :: Player -> Player
    nextPlayer player
      | player == currPlayer = turnPlayer dieScore player
      | otherwise = player

dice :: Scores
dice = map (\s -> (s, toInteger . length . filter (== s) $ sums)) . nub $ sums
  where
    values = [1, 2, 3]
    combinations = [[x, y, z] | x <- values, y <- values, z <- values]
    sums = map sum combinations

updateGame :: (Game, Integer) -> Games -> Games
updateGame (game, count) games = insert game (count + existingCount) games
  where
    existingCount = fromMaybe 0 . lookup game $ games

updateGames :: [(Game, Integer)] -> Games -> Games
updateGames gamesToUpdate games = foldl (flip updateGame) games gamesToUpdate

toName :: Player -> String
toName (name, _, _) = name

toPlayerWins :: [(Game, Integer)] -> PlayerWins
toPlayerWins = foldl merge (0, 0) . map each
  where
    each :: (Game, Integer) -> PlayerWins
    each (game, count)
      | player1Score > player2Score = (count, 0)
      | otherwise = (0, count)
      where
        player1Score = (* count) . toInteger . toScore . head . filter ((== "Player 1") . toName) $ game
        player2Score = (* count) . toInteger . toScore . head . filter ((== "Player 2") . toName) $ game

merge :: PlayerWins -> PlayerWins -> PlayerWins
merge (player1ScoreA, player2ScoreA) (player1ScoreB, player2ScoreB) = (player1ScoreA + player1ScoreB, player2ScoreA + player2ScoreB)

toScore :: Player -> Score
toScore (_, score, _) = score

universes :: Games -> PlayerWins
universes games
  | null games = (0, 0)
  | otherwise = gamesScores `merge` universes (updateGames notWonGames otherGames)
  where
    (game, count) = withCount . head . keys $ games :: (Game, Integer)
    otherGames = delete game games :: Games
    scores = dice :: Scores
    newGames = map (\(score, count) -> (turn score game, count)) $ scores :: [(Game, Integer)]
    wonGames = map (\(g, i) -> (g, i * count)) . filter (hasWinner . fst) $ newGames :: [(Game, Integer)]
    notWonGames = map (\(g, i) -> (g, i * count)) . filter (not . hasWinner . fst) $ newGames :: [(Game, Integer)]

    gamesScores = toPlayerWins wonGames :: PlayerWins

    withCount :: Game -> (Game, Integer)
    withCount game = (game, toCount game)

    toCount :: Game -> Integer
    toCount game = fromMaybe 0 . lookup game $ games

newPlayer :: (String, Space) -> Player
newPlayer (name, space) = (name, 0, space)

createGame :: [Player] -> Game
createGame players = players

howManyUniverses :: String -> Integer
howManyUniverses input
  | player1Wins >= player2Wins = player1Wins
  | otherwise = player2Wins
  where
    players = map newPlayer . parseInput $ input :: [Player]
    newGame = createGame players :: Game
    games = fromList [(newGame, 1)]
    (player1Wins, player2Wins) = universes games

{-# LANGUAGE TupleSections #-}

module Day23Part02Spec where

import Data.Map (fromList, toList)
import Day23 (Amphipods, Field, nextMoves, nextMovesOf, parseInput, performMove, possibleMovesOf)
import Day23Part02
import Day23Spec (testInput)
import Test.Hspec

testAmphipods = parseInput . extendInput $ testInput :: Amphipods

testField = (,edges) testAmphipods :: Field

testField1 = performMove ('D', ('D', 0), ('H', 10)) testField :: Field

testField2 = performMove ('A', ('D', 1), ('H', 0)) testField1 :: Field

testField3 = performMove ('B', ('C', 0), ('H', 9)) testField2 :: Field

testField4 = performMove ('B', ('C', 1), ('H', 7)) testField3 :: Field

testField5 = performMove ('A', ('C', 2), ('H', 1)) testField4 :: Field

testField6 = performMove ('C', ('B', 0), ('C', 2)) testField5 :: Field

testField7 = performMove ('C', ('B', 1), ('C', 1)) testField6 :: Field

testField8 = performMove ('B', ('B', 2), ('H', 5)) testField7 :: Field

testField9 = performMove ('D', ('B', 3), ('H', 3)) testField8 :: Field

testField10 = performMove ('B', ('H', 5), ('B', 3)) testField9 :: Field

testField11 = performMove ('B', ('H', 7), ('B', 2)) testField10 :: Field

spec :: Spec
spec = do
  it "extendInput" $ do
    extendInput testInput `shouldBe` "#############\n#...........#\n###B#C#B#D###\n  #D#C#B#A#\n  #D#B#A#C#\n  #A#D#C#A#\n  #########"

  it "parseInput" $ do
    testAmphipods `shouldBe` fromList [(('A', 0), 'B'), (('A', 1), 'D'), (('A', 2), 'D'), (('A', 3), 'A'), (('B', 0), 'C'), (('B', 1), 'C'), (('B', 2), 'B'), (('B', 3), 'D'), (('C', 0), 'B'), (('C', 1), 'B'), (('C', 2), 'A'), (('C', 3), 'C'), (('D', 0), 'D'), (('D', 1), 'A'), (('D', 2), 'C'), (('D', 3), 'A')]

  it "possibleMovesOf" $ do
    possibleMovesOf ('H', 10) testField2 `shouldBe` []
    possibleMovesOf ('B', 0) testField5 `shouldBe` [('C', ('B', 0), ('H', 3)), ('C', ('B', 0), ('H', 5)), ('C', ('B', 0), ('C', 2))]
    possibleMovesOf ('C', 3) testField5 `shouldBe` []
    possibleMovesOf ('B', 2) testField7 `shouldBe` [('B', ('B', 2), ('H', 3)), ('B', ('B', 2), ('H', 5))]
    possibleMovesOf ('D', 2) testField11 `shouldBe` [('C', ('D', 2), ('H', 7)), ('C', ('D', 2), ('H', 5)), ('C', ('D', 2), ('C', 0))]

  it "nextMovesOf" $ do
    nextMovesOf ('H', 10) testField2 `shouldBe` []
    nextMovesOf ('B', 0) testField5 `shouldBe` [('C', ('B', 0), ('C', 2))]
    nextMovesOf ('D', 2) testField11 `shouldBe` [('C', ('D', 2), ('C', 0))]

  it "nextMoves" $ do
    (length . nextMoves $ testField) `shouldBe` 28
    (length . nextMoves $ testField1) `shouldBe` 24
    (length . nextMoves $ testField2) `shouldBe` 20
    (length . nextMoves $ testField3) `shouldBe` 16
    (length . nextMoves $ testField4) `shouldBe` 9
    (length . nextMoves $ testField5) `shouldBe` 3
    (length . nextMoves $ testField6) `shouldBe` 3
    (length . nextMoves $ testField7) `shouldBe` 4
    (length . nextMoves $ testField8) `shouldBe` 2
    (length . nextMoves $ testField9) `shouldBe` 1
    (length . nextMoves $ testField10) `shouldBe` 1
    (length . nextMoves $ testField11) `shouldBe` 2

  it "leastEnergyRequiredExtended" $ do
    leastEnergyRequiredExtended testInput `shouldBe` 44169

main = hspec spec

{-# LANGUAGE TupleSections #-}

module Day23Spec where

import Data.Map (empty, fromList)
import Day23
import GHC.Arr (array)
import Test.Hspec

testInput = "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########\n"

productiveInput = "#############\n#...........#\n###A#C#B#C###\n  #D#A#D#B#\n  #########"

completedField =
  (,edges) $
    fromList
      [ (('A', 0), 'A'),
        (('A', 1), 'A'),
        (('B', 0), 'B'),
        (('B', 1), 'B'),
        (('C', 0), 'C'),
        (('C', 1), 'C'),
        (('D', 0), 'D'),
        (('D', 1), 'D')
      ] ::
    Field

deadField =
  (,edges) $
    fromList
      [ (('H', 1), 'C'),
        (('H', 3), 'D'),
        (('H', 5), 'C'),
        (('H', 7), 'A'),
        (('H', 9), 'B'),
        (('A', 1), 'D'),
        (('C', 1), 'B'),
        (('D', 1), 'A')
      ] ::
    Field

almostCompletedField =
  (,edges) $
    fromList
      [ (('A', 0), 'A'),
        (('A', 1), 'A'),
        (('B', 0), 'B'),
        (('B', 1), 'B'),
        (('C', 0), 'C'),
        (('C', 1), 'C'),
        (('H', 9), 'D'),
        (('D', 1), 'D')
      ] ::
    Field

testAmphopids = parseInput testInput

testField = (,edges) testAmphopids :: Field

testField1 = performMove ('B', ('C', 0), ('H', 3)) testField :: Field

testField2 = performMove ('C', ('B', 0), ('C', 0)) testField1 :: Field

testField3 = performMove ('D', ('B', 1), ('H', 5)) testField2 :: Field

testField4 = performMove ('B', ('H', 3), ('B', 1)) testField3 :: Field

testField5 = performMove ('B', ('A', 0), ('B', 0)) testField4 :: Field

testField6 = performMove ('D', ('D', 0), ('H', 7)) testField5 :: Field

testField7 = performMove ('A', ('D', 1), ('H', 9)) testField6 :: Field

spec :: Spec
spec = do
  let (testGraph, _, _) = createGraph testField

  it "parseInput" $ do
    fst testField `shouldBe` fromList [(('A', 0), 'B'), (('A', 1), 'A'), (('B', 0), 'C'), (('B', 1), 'D'), (('C', 0), 'B'), (('C', 1), 'C'), (('D', 0), 'D'), (('D', 1), 'A')]
    testGraph `shouldBe` array (0, 18) [(0, [1, 10]), (1, [0]), (2, [3, 12]), (3, [2]), (4, [5, 14]), (5, [4]), (6, [7, 16]), (7, [6]), (8, [9]), (9, [8, 10]), (10, [9, 11, 0]), (11, [10, 12]), (12, [11, 13, 2]), (13, [12, 14]), (14, [13, 15, 4]), (15, [14, 16]), (16, [15, 17, 6]), (17, [16, 18]), (18, [17])]

  it "possibleTargets" $ do
    possibleTargets ('A', 0) testField
      `shouldBe` [('H', 1), ('H', 0), ('H', 3), ('H', 5), ('H', 7), ('H', 9), ('H', 10)]
    possibleTargets ('A', 1) testField
      `shouldBe` []
    possibleTargets ('B', 0) testField
      `shouldBe` [('H', 3), ('H', 1), ('H', 0), ('H', 5), ('H', 7), ('H', 9), ('H', 10)]

  it "possibleMovesOf" $ do
    possibleMovesOf ('A', 0) testField
      `shouldBe` [ ('B', ('A', 0), ('H', 1)),
                   ('B', ('A', 0), ('H', 0)),
                   ('B', ('A', 0), ('H', 3)),
                   ('B', ('A', 0), ('H', 5)),
                   ('B', ('A', 0), ('H', 7)),
                   ('B', ('A', 0), ('H', 9)),
                   ('B', ('A', 0), ('H', 10))
                 ]
  it "possibleMovesOf after two moves" $ do
    let newField = performMove ('D', ('D', 0), ('H', 9)) . performMove ('B', ('A', 0), ('H', 1)) $ testField
    possibleMovesOf ('D', 1) newField
      `shouldBe` [ ('A', ('D', 1), ('H', 7)),
                   ('A', ('D', 1), ('H', 5)),
                   ('A', ('D', 1), ('H', 3)),
                   ('A', ('D', 1), ('A', 0))
                 ]
    possibleMovesOf ('A', 1) newField
      `shouldBe` []

  it "performMove" $ do
    (fst . performMove ('B', ('A', 0), ('H', 1)) $ testField)
      `shouldBe` fromList
        [ (('A', 1), 'A'),
          (('B', 0), 'C'),
          (('B', 1), 'D'),
          (('C', 0), 'B'),
          (('C', 1), 'C'),
          (('D', 0), 'D'),
          (('D', 1), 'A'),
          (('H', 1), 'B')
        ]

  it "performMoves" $ do
    (fst . performMove ('D', ('D', 0), ('H', 9)) . performMove ('B', ('A', 0), ('H', 1)) $ testField)
      `shouldBe` fromList
        [ (('A', 1), 'A'),
          (('B', 0), 'C'),
          (('B', 1), 'D'),
          (('C', 0), 'B'),
          (('C', 1), 'C'),
          (('H', 9), 'D'),
          (('D', 1), 'A'),
          (('H', 1), 'B')
        ]

  it "nextMovesOf" $ do
    let newField = performMove ('D', ('D', 0), ('H', 9)) . performMove ('B', ('A', 0), ('H', 1)) $ testField
    nextMovesOf ('D', 1) newField `shouldBe` [('A', ('D', 1), ('A', 0))]

  it "nextMoves" $ do
    nextMoves testField
      `shouldBe` [ ('B', ('A', 0), ('H', 1)),
                   ('B', ('A', 0), ('H', 0)),
                   ('B', ('A', 0), ('H', 3)),
                   ('B', ('A', 0), ('H', 5)),
                   ('B', ('A', 0), ('H', 7)),
                   ('B', ('A', 0), ('H', 9)),
                   ('B', ('A', 0), ('H', 10)),
                   ('C', ('B', 0), ('H', 3)),
                   ('C', ('B', 0), ('H', 1)),
                   ('C', ('B', 0), ('H', 0)),
                   ('C', ('B', 0), ('H', 5)),
                   ('C', ('B', 0), ('H', 7)),
                   ('C', ('B', 0), ('H', 9)),
                   ('C', ('B', 0), ('H', 10)),
                   ('B', ('C', 0), ('H', 5)),
                   ('B', ('C', 0), ('H', 3)),
                   ('B', ('C', 0), ('H', 1)),
                   ('B', ('C', 0), ('H', 0)),
                   ('B', ('C', 0), ('H', 7)),
                   ('B', ('C', 0), ('H', 9)),
                   ('B', ('C', 0), ('H', 10)),
                   ('D', ('D', 0), ('H', 7)),
                   ('D', ('D', 0), ('H', 5)),
                   ('D', ('D', 0), ('H', 3)),
                   ('D', ('D', 0), ('H', 1)),
                   ('D', ('D', 0), ('H', 0)),
                   ('D', ('D', 0), ('H', 9)),
                   ('D', ('D', 0), ('H', 10))
                 ]

    nextMoves deadField `shouldBe` []
    nextMoves completedField `shouldBe` []
    nextMoves almostCompletedField `shouldBe` [('D', ('H', 9), ('D', 0))]
    (length . nextMoves $ testField) `shouldBe` 28
    (length . nextMoves . performMove ('D', ('D', 0), ('H', 9)) $ testField) `shouldBe` 20
    (length . nextMoves $ testField1) `shouldBe` 2 + 1 + 4
    (length . nextMoves $ testField2) `shouldBe` 2 + 4 + 4
    (length . nextMoves $ testField4) `shouldBe` 1 + 3
    nextMoves testField7 `shouldBe` [('D', ('H', 7), ('D', 1))]
    (length . nextMoves $ testField7) `shouldBe` 1

  it "movesIntoTarget" $ do
    movesIntoTarget ('D', ('H', 7), ('D', 1)) `shouldBe` True

  it "nextMovesOf" $ do
    nextMovesOf ('H', 7) testField7 `shouldBe` [('D', ('H', 7), ('D', 1))]

  it "possibleMovesOf" $ do
    possibleMovesOf ('H', 7) testField7 `shouldBe` [('D', ('H', 7), ('D', 1))]

  it "possibleTargets" $ do
    possibleTargets ('H', 7) testField7 `shouldBe` [('D', 1)]

  it "hasBeenCompleted" $ do
    hasBeenCompleted testField `shouldBe` False
    (hasBeenCompleted $ completedField) `shouldBe` True
    (hasBeenCompleted $ almostCompletedField) `shouldBe` False
    hasBeenCompleted (performMove ('D', ('H', 9), ('D', 0)) $ almostCompletedField) `shouldBe` True

  it "isDead" $ do
    isDead completedField `shouldBe` True
    isDead deadField `shouldBe` True
    isDead testField `shouldBe` False
    isDead almostCompletedField `shouldBe` False

  it "search" $ do
    search testField `shouldBe` 12521
    search ((,edges) . parseInput $ productiveInput) `shouldBe` 16300

  it "leastEnergyRequired" $ do
    leastEnergyRequired testInput `shouldBe` 12521
    leastEnergyRequired productiveInput `shouldBe` 16300

main = hspec spec

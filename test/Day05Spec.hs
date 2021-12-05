module Day05Spec where

import Day05
import Test.Hspec

testInput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` [ ((0, 9), (5, 9)),
                   ((8, 0), (0, 8)),
                   ((9, 4), (3, 4)),
                   ((2, 2), (2, 1)),
                   ((7, 0), (7, 4)),
                   ((6, 4), (2, 0)),
                   ((0, 9), (2, 9)),
                   ((3, 4), (1, 4)),
                   ((0, 0), (8, 8)),
                   ((5, 5), (8, 2))
                 ]

  it "extract" $ do
    extract ((1, 1), (1, 3)) `shouldBe` [(1, 1), (1, 2), (1, 3)]
    extract ((9, 7), (7, 7)) `shouldBe` [(9, 7), (8, 7), (7, 7)]
    extract ((1, 3), (1, 1)) `shouldBe` [(1, 3), (1, 2), (1, 1)]
    extract ((7, 7), (9, 7)) `shouldBe` [(7, 7), (8, 7), (9, 7)]

  it "overlap" $ do
    overlap testInput `shouldBe` 5

main = hspec spec

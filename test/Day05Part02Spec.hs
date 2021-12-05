module Day05Part02Spec where

import Day05 (parseInput)
import Day05Part02
import Debug.Trace (traceShow)
import Test.Hspec

testInput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

spec :: Spec
spec = do
  it "extract" $ do
    extract ((1, 1), (1, 1)) `shouldBe` [(1, 1)]
    extract ((1, 1), (1, 3)) `shouldBe` [(1, 1), (1, 2), (1, 3)]
    extract ((9, 7), (7, 7)) `shouldBe` [(7, 7), (8, 7), (9, 7)]
    extract ((1, 3), (1, 1)) `shouldBe` [(1, 1), (1, 2), (1, 3)]
    extract ((7, 7), (9, 7)) `shouldBe` [(7, 7), (8, 7), (9, 7)]
    extract ((1, 1), (3, 3)) `shouldBe` [(1, 1), (2, 2), (3, 3)]
    extract ((3, 3), (1, 1)) `shouldBe` [(1, 1), (2, 2), (3, 3)]
    extract ((9, 7), (7, 9)) `shouldBe` [(7, 9), (8, 8), (9, 7)]
    extract ((7, 9), (9, 7)) `shouldBe` [(7, 9), (8, 8), (9, 7)]
    extract ((7, 7), (9, 9)) `shouldBe` [(7, 7), (8, 8), (9, 9)]
    extract ((9, 9), (7, 7)) `shouldBe` [(7, 7), (8, 8), (9, 9)]
    extract ((9, 9), (7, 6)) `shouldBe` []
    extract ((0, 9), (5, 9)) `shouldBe` [(0, 9), (1, 9), (2, 9), (3, 9), (4, 9), (5, 9)]
    extract ((8, 0), (0, 8)) `shouldBe` [(0, 8), (1, 7), (2, 6), (3, 5), (4, 4), (5, 3), (6, 2), (7, 1), (8, 0)]
    extract ((9, 4), (3, 4)) `shouldBe` [(3, 4), (4, 4), (5, 4), (6, 4), (7, 4), (8, 4), (9, 4)]
    extract ((2, 2), (2, 1)) `shouldBe` [(2, 1), (2, 2)]
    extract ((7, 0), (7, 4)) `shouldBe` [(7, 0), (7, 1), (7, 2), (7, 3), (7, 4)]
    extract ((6, 4), (2, 0)) `shouldBe` [(2, 0), (3, 1), (4, 2), (5, 3), (6, 4)]
    extract ((0, 9), (2, 9)) `shouldBe` [(0, 9), (1, 9), (2, 9)]
    extract ((3, 4), (1, 4)) `shouldBe` [(1, 4), (2, 4), (3, 4)]
    extract ((0, 0), (8, 8)) `shouldBe` [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8)]
    extract ((5, 5), (8, 2)) `shouldBe` [(5, 5), (6, 4), (7, 3), (8, 2)]

  it "overlapDiagonal" $ do
    overlapDiagonal testInput `shouldBe` 12

main = hspec spec

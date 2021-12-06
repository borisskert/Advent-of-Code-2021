module Day06Spec where

import Day06
import Test.Hspec

testInput = "3,4,3,1,2"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput `shouldBe` [3, 4, 3, 1, 2]

  it "lanternfish" $ do
    lanternfish 0 [3, 4, 3, 1, 2] `shouldBe` [3, 4, 3, 1, 2]
    lanternfish 1 [3, 4, 3, 1, 2] `shouldBe` [2, 3, 2, 0, 1]
    lanternfish 1 [2, 3, 2, 0, 1] `shouldBe` [1, 2, 1, 6, 0, 8]
    lanternfish 2 [3, 4, 3, 1, 2] `shouldBe` [1, 2, 1, 6, 0, 8]
    lanternfish 18 [3, 4, 3, 1, 2] `shouldBe` [6, 0, 6, 4, 5, 6, 0, 1, 1, 2, 6, 0, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 8, 8, 8]

  it "simulateLanternfish" $ do
    simulateLanternfish 18 testInput `shouldBe` 26
    simulateLanternfish 80 testInput `shouldBe` 5934

main = hspec spec

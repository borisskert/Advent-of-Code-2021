module Day06Part02Spec where

import Day06Part02
import Test.Hspec

testInput = "3,4,3,1,2"

spec :: Spec
spec = do
  it "initialLaternfish" $ do
    laternfish 0 0 `shouldBe` 1
    laternfish 1 0 `shouldBe` 2
    laternfish 5 0 `shouldBe` 2
    laternfish 6 0 `shouldBe` 2
    laternfish 7 0 `shouldBe` 2
    laternfish 9 0 `shouldBe` 3
    laternfish 10 0 `shouldBe` 4
    laternfish 12 0 `shouldBe` 4
    laternfish 13 0 `shouldBe` 4
    laternfish 15 0 `shouldBe` 5
    laternfish 17 0 `shouldBe` 7
    laternfish 19 0 `shouldBe` 8

  it "simulateLanternfishExtended" $ do
    simulateLanternfishExtended 0 testInput `shouldBe` 5
    simulateLanternfishExtended 1 testInput `shouldBe` 5
    simulateLanternfishExtended 2 testInput `shouldBe` 6
    simulateLanternfishExtended 3 testInput `shouldBe` 7
    simulateLanternfishExtended 4 testInput `shouldBe` 9
    simulateLanternfishExtended 5 testInput `shouldBe` 10
    simulateLanternfishExtended 6 testInput `shouldBe` 10
    simulateLanternfishExtended 7 testInput `shouldBe` 10
    simulateLanternfishExtended 8 testInput `shouldBe` 10
    simulateLanternfishExtended 9 testInput `shouldBe` 11
    simulateLanternfishExtended 10 testInput `shouldBe` 12
    simulateLanternfishExtended 18 testInput `shouldBe` 26
    simulateLanternfishExtended 80 testInput `shouldBe` 5934
    simulateLanternfishExtended 256 testInput `shouldBe` 26984457539

main = hspec spec

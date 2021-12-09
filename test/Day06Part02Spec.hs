module Day06Part02Spec where

import Day06Part02
import Test.Hspec

testInput = "3,4,3,1,2"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput `shouldBe` [(3, 2), (4, 1), (1, 1), (2, 1)]

  it "laternfish" $ do
    laternfish 0 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(3, 2), (4, 1), (1, 1), (2, 1)]
    laternfish 1 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(2, 2), (3, 1), (0, 1), (1, 1)]
    laternfish 2 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(1, 2), (2, 1), (0, 1), (6, 1), (8, 1)]
    laternfish 3 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(0, 2), (1, 1), (5, 1), (7, 1), (6, 1), (8, 1)]
    laternfish 4 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(0, 1), (4, 1), (5, 1), (7, 1), (6, 3), (8, 2)]
    laternfish 5 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(3, 1), (4, 1), (5, 3), (7, 2), (6, 2), (8, 1)]
    laternfish 6 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(2, 1), (3, 1), (4, 3), (6, 2), (5, 2), (7, 1)]
    laternfish 7 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(1, 1), (2, 1), (3, 3), (5, 2), (4, 2), (6, 1)]
    laternfish 8 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(0, 1), (1, 1), (2, 3), (4, 2), (3, 2), (5, 1)]
    laternfish 9 [(3, 2), (4, 1), (1, 1), (2, 1)] `shouldBe` [(0, 1), (1, 3), (3, 2), (2, 2), (4, 1), (6, 1), (8, 1)]

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

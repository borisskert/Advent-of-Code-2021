module Day21Spec where

import Day21
import Test.Hspec

testInput = "Player 1 starting position: 4\nPlayer 2 starting position: 8"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput `shouldBe` [("Player 1", 4), ("Player 2", 8)]

  it "losingSituation" $ do
    losingSituation testInput `shouldBe` (745, 993)

main = hspec spec

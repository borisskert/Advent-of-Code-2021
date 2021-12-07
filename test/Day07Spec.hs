module Day07Spec where

import Day07
import Test.Hspec

testInput = "16,1,2,0,4,2,7,1,2,14"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput `shouldBe` [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

  it "cheapestPossibleOutcome" $ do
    cheapestPossibleOutcome testInput `shouldBe` 37

main = hspec spec

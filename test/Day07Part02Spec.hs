module Day07Part02Spec where

import Day07Part02
import Test.Hspec

testInput = "16,1,2,0,4,2,7,1,2,14"

spec :: Spec
spec = do
  it "fuelCosts" $ do
    fuelCosts 0 `shouldBe` 0
    fuelCosts 1 `shouldBe` 1
    fuelCosts 2 `shouldBe` 1 + 2
    fuelCosts 3 `shouldBe` 1 + 2 + 3

  it "cheapestPossibleOutcomeExtended" $ do
    cheapestPossibleOutcomeExtended testInput `shouldBe` 168

main = hspec spec

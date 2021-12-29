module Day18Part02Spec where

import Day18Part02
import Day18Spec (exampleHomework)
import Test.Hspec

spec :: Spec
spec = do
  it "largestMagnitude" $ do
    largestMagnitude exampleHomework `shouldBe` 3993

main = hspec spec

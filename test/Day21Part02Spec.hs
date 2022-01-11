module Day21Part02Spec where

import Day21Part02
import Day21Spec (testInput)
import Test.Hspec

spec :: Spec
spec = do
  it "howManyUniverses" $ do
    howManyUniverses testInput `shouldBe` 444356092776315

main = hspec spec

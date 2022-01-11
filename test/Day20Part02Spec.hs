module Day20Part02Spec where

import Day20Part02
import Day20Spec (testInput)
import Test.Hspec

spec :: Spec
spec = do
  it "howManyPixelsAreLitExtended" $ do
    howManyPixelsAreLitExtended testInput 2 `shouldBe` 35
    howManyPixelsAreLitExtended testInput 50 `shouldBe` 3351

main = hspec spec

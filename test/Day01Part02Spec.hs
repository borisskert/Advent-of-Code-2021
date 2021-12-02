module Day01Part02Spec where

import Test.Hspec
import Day01Part02


spec :: Spec
spec = do
  it "Example" $ do
    countIncreasedSums "199\n200\n208\n210\n200\n207\n240\n269\n260\n263" `shouldBe` 5
  it "Example with ending newline" $ do
    countIncreasedSums "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n" `shouldBe` 5


main = hspec spec

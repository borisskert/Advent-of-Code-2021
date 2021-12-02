module Day02Spec where

import Day02
import Test.Hspec


spec :: Spec
spec = do
  it "Example" $ do
    submarineDrive "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2" `shouldBe` (15, 10)


main = hspec spec

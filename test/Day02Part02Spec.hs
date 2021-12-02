module Day02Part02Spec where

import Day02Part02
import Test.Hspec


spec :: Spec
spec = do
  it "Example" $ do
    submarineDriveAim "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2" `shouldBe` (15, 60)


main = hspec spec

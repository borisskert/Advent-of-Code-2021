module Day17Part02Spec where

import Day17Part02
import Test.Hspec

testInput = "target area: x=20..30, y=-10..-5"

spec :: Spec
spec = do
  
  it "howManyDifferentVelocities" $ do
    howManyDifferentVelocities testInput `shouldBe` 112

main = hspec spec

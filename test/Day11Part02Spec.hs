module Day11Part02Spec where

import Day11Part02
import Test.Hspec

testInput = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

spec :: Spec
spec = do
  it "flashSimultaneously" $ do
    flashSimultaneously testInput `shouldBe` 195

main = hspec spec

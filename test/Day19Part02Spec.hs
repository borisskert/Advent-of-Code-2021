module Day19Part02Spec where

import Day19Part02
import Day19Spec (testInput)
import Test.Hspec

spec :: Spec
spec = do
  it "largestManhattanDistance" $ do
    largestManhattanDistance testInput `shouldBe` 3621

main = hspec spec

module Day01Spec where

import Test.Hspec
import Day01


spec :: Spec
spec = do
  it "One increasing" $ do
    sonarSweep "199\n200" `shouldBe` 1
  it "Two increasing" $ do
    sonarSweep "199\n200\n208" `shouldBe` 2
  it "Three increasing" $ do
    sonarSweep "199\n200\n208\n210\n" `shouldBe` 3
  it "Three increasing one ignored" $ do
    sonarSweep "199\n200\n208\n210\n200" `shouldBe` 3
  it "Four increasing one ignored between" $ do
    sonarSweep "199\n200\n208\n210\n200\n207\n" `shouldBe` 4
  it "Five increasing one ignored between" $ do
    sonarSweep "199\n200\n208\n210\n200\n207\n240" `shouldBe` 5
  it "Example" $ do
    sonarSweep "199\n200\n208\n210\n200\n207\n240\n269\n260\n263" `shouldBe` 7
  it "Example with ending newline" $ do
    sonarSweep "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n" `shouldBe` 7


main = hspec spec

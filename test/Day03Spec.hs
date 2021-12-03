module Day03Spec where

import Day03
import Test.Hspec

testInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
testInputAsList = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

spec :: Spec
spec = do
  it "mostCommonBits" $ do
    mostCommonBits testInputAsList `shouldBe` "10110"

  it "mostCommonBitAt" $ do
    mostCommonBitAt 0 testInputAsList `shouldBe` '1'
    mostCommonBitAt 1 testInputAsList `shouldBe` '0'
    mostCommonBitAt 2 testInputAsList `shouldBe` '1'
    mostCommonBitAt 3 testInputAsList `shouldBe` '1'
    mostCommonBitAt 4 testInputAsList `shouldBe` '0'

  it "flip" $ do
    flipBits "10110" `shouldBe` "01001"

  it "binToInt" $ do
    binToInt "10110" `shouldBe` 22
    binToInt "01001" `shouldBe` 9

  it "powerConsumption" $ do
    powerConsumption testInput `shouldBe` (22, 9)
    
main = hspec spec

module Day03Part02Spec where


import Day03Part02
import Test.Hspec

testInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
testInputAsList = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

spec :: Spec
spec = do
  it "mostCommonBitAt" $ do
    mostCommonBitAt 0 testInputAsList `shouldBe` Just '1'
    mostCommonBitAt 1 testInputAsList `shouldBe` Just '0'
    mostCommonBitAt 2 testInputAsList `shouldBe` Just '1'
    mostCommonBitAt 3 testInputAsList `shouldBe` Just '1'
    mostCommonBitAt 4 testInputAsList `shouldBe` Just '0'
    mostCommonBitAt 0 ["1110","0110","0111","0101","1100","0000","1001"] `shouldBe` Just '0'
    mostCommonBitAt 0 ["0","1"] `shouldBe` Nothing

  it "leastCommonBitAt" $ do
    leastCommonBitAt 0 testInputAsList `shouldBe` Just '0'
    leastCommonBitAt 1 testInputAsList `shouldBe` Just '1'
    leastCommonBitAt 2 testInputAsList `shouldBe` Just '0'
    leastCommonBitAt 3 testInputAsList `shouldBe` Just '0'
    leastCommonBitAt 4 testInputAsList `shouldBe` Just '1'

  it "binToInt" $ do
    binToInt "10110" `shouldBe` 22
    binToInt "01001" `shouldBe` 9

  it "oxygenGeneratorRating" $ do
    oxygenGeneratorRating testInputAsList `shouldBe` "10111"

  it "co2ScrubberRating" $ do
    co2ScrubberRating testInputAsList `shouldBe` "01010"

  it "powerConsumption" $ do
    lifeSupportRating testInput `shouldBe` (23, 10)
    
main = hspec spec

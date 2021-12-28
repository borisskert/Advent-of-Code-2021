module Day15Spec where

import Data.Map (fromList)
import Day15
import Test.Hspec

testInput = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` [ [1, 1, 6, 3, 7, 5, 1, 7, 4, 2],
                   [1, 3, 8, 1, 3, 7, 3, 6, 7, 2],
                   [2, 1, 3, 6, 5, 1, 1, 3, 2, 8],
                   [3, 6, 9, 4, 9, 3, 1, 5, 6, 9],
                   [7, 4, 6, 3, 4, 1, 7, 1, 1, 1],
                   [1, 3, 1, 9, 1, 2, 8, 1, 3, 7],
                   [1, 3, 5, 9, 9, 1, 2, 4, 2, 1],
                   [3, 1, 2, 5, 4, 2, 1, 6, 3, 9],
                   [1, 2, 9, 3, 1, 3, 8, 5, 2, 1],
                   [2, 3, 1, 1, 9, 4, 4, 5, 8, 1]
                 ]

  it "lowestRiskLevel" $ do
    lowestRiskLevel [[1, 1, 6, 3], [1, 3, 8, 1], [2, 1, 3, 6], [3, 6, 9, 4]] `shouldBe` 17
    lowestRiskLevel [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` 20
    (lowestRiskLevel . parseInput $ testInput) `shouldBe` 40
    lowestRiskLevel [[1,1,1,9,9], [9,9,1,9,9],[9,1,1,9,9],[9,1,9,9,9],[9,1,1,1,1]] `shouldBe` 10

  it "lowestTotal" $ do
    lowestTotal "123\n456\n789" `shouldBe` 20
    lowestTotal "1234\n5678\n9012\n3456" `shouldBe` 17
    lowestTotal "12345\n67890\n12345\n67890\n12345" `shouldBe` 24
    lowestTotal "123456\n789012\n345678\n901234\n567890\n123456" `shouldBe` 30
    lowestTotal "1234567\n8901234\n5678901\n2345678\n9012345\n6789012\n3456789" `shouldBe` 34
    lowestTotal "12345678\n90123456\n78901234\n56789012\n34567890\n12345678\n90123456\n78901234" `shouldBe` 29
    lowestTotal "123456789\n012345678\n901234567\n890123456\n789012345\n678901234\n567890123\n456789012\n345678901" `shouldBe` 8
    lowestTotal testInput `shouldBe` 40

main = hspec spec

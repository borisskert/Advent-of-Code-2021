module Day13Part02Spec where

import Day13Part02
import Test.Hspec

testInput = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"

spec :: Spec
spec = do
  it "foldByAll" $ do
    foldByAll (10, 12) [('y', 7), ('x', 5)] `shouldBe` Just (0, 2)

  it "howManyDots" $ do
    howManyDots testInput `shouldBe` 16

  it "dots" $ do
    dots testInput `shouldBe` [(4, 4), (0, 0), (1, 4), (0, 3), (0, 4), (4, 3), (4, 0), (4, 2), (4, 1), (0, 1), (0, 2), (3, 4), (3, 0), (2, 4), (2, 0), (1, 0)]

  it "printDots" $ do
    printDots testInput `shouldBe` "#####\n#...#\n#...#\n#...#\n#####\n"

main = hspec spec

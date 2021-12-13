module Day13Spec where

import Day13
import Test.Hspec

testInput = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` ( [ (6, 10),
                     (0, 14),
                     (9, 10),
                     (0, 3),
                     (10, 4),
                     (4, 11),
                     (6, 0),
                     (6, 12),
                     (4, 1),
                     (0, 13),
                     (10, 12),
                     (3, 4),
                     (3, 0),
                     (8, 4),
                     (1, 10),
                     (2, 14),
                     (8, 10),
                     (9, 0)
                   ],
                   [('y', 7), ('x', 5)]
                 )

  it "foldBy" $ do
    foldBy (0, 0) ('y', 7) `shouldBe` Just (0, 0)
    foldBy (6, 6) ('y', 7) `shouldBe` Just (6, 6)
    foldBy (7, 7) ('y', 7) `shouldBe` Nothing
    foldBy (7, 8) ('y', 7) `shouldBe` Just (7, 6)
    foldBy (7, 9) ('y', 7) `shouldBe` Just (7, 5)
    foldBy (7, 10) ('y', 7) `shouldBe` Just (7, 4)
    foldBy (7, 11) ('y', 7) `shouldBe` Just (7, 3)
    foldBy (7, 12) ('y', 7) `shouldBe` Just (7, 2)
    foldBy (7, 13) ('y', 7) `shouldBe` Just (7, 1)
    foldBy (7, 14) ('y', 7) `shouldBe` Just (7, 0)

    foldBy (0, 0) ('x', 7) `shouldBe` Just (0, 0)
    foldBy (6, 6) ('x', 7) `shouldBe` Just (6, 6)
    foldBy (7, 7) ('x', 7) `shouldBe` Nothing
    foldBy (8, 7) ('x', 7) `shouldBe` Just (6, 7)
    foldBy (9, 7) ('x', 7) `shouldBe` Just (5, 7)
    foldBy (10, 7) ('x', 7) `shouldBe` Just (4, 7)
    foldBy (11, 7) ('x', 7) `shouldBe` Just (3, 7)
    foldBy (12, 7) ('x', 7) `shouldBe` Just (2, 7)
    foldBy (13, 7) ('x', 7) `shouldBe` Just (1, 7)
    foldBy (14, 7) ('x', 7) `shouldBe` Just (0, 7)

    foldBy (10, 12) ('y', 7) `shouldBe` Just (10, 2)
    foldBy (10, 2) ('x', 5) `shouldBe` Just (0, 2)

  it "howManyDots" $ do
    howManyDots testInput `shouldBe` 17

main = hspec spec

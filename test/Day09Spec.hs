module Day09Spec where

import Day09
import Test.Hspec

testInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

testPoints =
  [ (Point {x = 0, y = 0}, 2),
    (Point {x = 0, y = 1}, 1),
    (Point {x = 0, y = 2}, 9),
    (Point {x = 0, y = 3}, 9),
    (Point {x = 0, y = 4}, 9),
    (Point {x = 0, y = 5}, 4),
    (Point {x = 0, y = 6}, 3),
    (Point {x = 0, y = 7}, 2),
    (Point {x = 0, y = 8}, 1),
    (Point {x = 0, y = 9}, 0),
    (Point {x = 1, y = 0}, 3),
    (Point {x = 1, y = 1}, 9),
    (Point {x = 1, y = 2}, 8),
    (Point {x = 1, y = 3}, 7),
    (Point {x = 1, y = 4}, 8),
    (Point {x = 1, y = 5}, 9),
    (Point {x = 1, y = 6}, 4),
    (Point {x = 1, y = 7}, 9),
    (Point {x = 1, y = 8}, 2),
    (Point {x = 1, y = 9}, 1),
    (Point {x = 2, y = 0}, 9),
    (Point {x = 2, y = 1}, 8),
    (Point {x = 2, y = 2}, 5),
    (Point {x = 2, y = 3}, 6),
    (Point {x = 2, y = 4}, 7),
    (Point {x = 2, y = 5}, 8),
    (Point {x = 2, y = 6}, 9),
    (Point {x = 2, y = 7}, 8),
    (Point {x = 2, y = 8}, 9),
    (Point {x = 2, y = 9}, 2),
    (Point {x = 3, y = 0}, 8),
    (Point {x = 3, y = 1}, 7),
    (Point {x = 3, y = 2}, 6),
    (Point {x = 3, y = 3}, 7),
    (Point {x = 3, y = 4}, 8),
    (Point {x = 3, y = 5}, 9),
    (Point {x = 3, y = 6}, 6),
    (Point {x = 3, y = 7}, 7),
    (Point {x = 3, y = 8}, 8),
    (Point {x = 3, y = 9}, 9),
    (Point {x = 4, y = 0}, 9),
    (Point {x = 4, y = 1}, 8),
    (Point {x = 4, y = 2}, 9),
    (Point {x = 4, y = 3}, 9),
    (Point {x = 4, y = 4}, 9),
    (Point {x = 4, y = 5}, 6),
    (Point {x = 4, y = 6}, 5),
    (Point {x = 4, y = 7}, 6),
    (Point {x = 4, y = 8}, 7),
    (Point {x = 4, y = 9}, 8)
  ]

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` testPoints

  it "lowPoints" $ do
    lowPoints
      testPoints
      `shouldBe` [ (Point {x = 0, y = 1}, 1),
                   (Point {x = 0, y = 9}, 0),
                   (Point {x = 2, y = 2}, 5),
                   (Point {x = 4, y = 6}, 5)
                 ]

  it "riskLevel" $ do
    riskLevel testInput `shouldBe` 15

main = hspec spec

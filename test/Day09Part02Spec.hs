module Day09Part02Spec where

import Data.Map (Map, fromList)
import Day09Part02
import Day09Part02 (Field (Field), Position (Position), Size (Size))
import Test.Hspec

testInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

testField =
  Field
    { size = Size {width = 10, height = 5},
      points =
        fromList
          [ (Position {x = 0, y = 0}, Point {position = Position {x = 0, y = 0}, value = 2, neighbors = [Position {x = 0, y = 1}, Position {x = 1, y = 0}]}),
            (Position {x = 0, y = 1}, Point {position = Position {x = 0, y = 1}, value = 3, neighbors = [Position {x = 0, y = 0}, Position {x = 0, y = 2}, Position {x = 1, y = 1}]}),
            (Position {x = 0, y = 2}, Point {position = Position {x = 0, y = 2}, value = 9, neighbors = [Position {x = 0, y = 1}, Position {x = 0, y = 3}, Position {x = 1, y = 2}]}),
            (Position {x = 0, y = 3}, Point {position = Position {x = 0, y = 3}, value = 8, neighbors = [Position {x = 0, y = 2}, Position {x = 0, y = 4}, Position {x = 1, y = 3}]}),
            (Position {x = 0, y = 4}, Point {position = Position {x = 0, y = 4}, value = 9, neighbors = [Position {x = 0, y = 3}, Position {x = 1, y = 4}]}),
            (Position {x = 1, y = 0}, Point {position = Position {x = 1, y = 0}, value = 1, neighbors = [Position {x = 0, y = 0}, Position {x = 1, y = 1}, Position {x = 2, y = 0}]}),
            (Position {x = 1, y = 1}, Point {position = Position {x = 1, y = 1}, value = 9, neighbors = [Position {x = 0, y = 1}, Position {x = 1, y = 0}, Position {x = 1, y = 2}, Position {x = 2, y = 1}]}),
            (Position {x = 1, y = 2}, Point {position = Position {x = 1, y = 2}, value = 8, neighbors = [Position {x = 0, y = 2}, Position {x = 1, y = 1}, Position {x = 1, y = 3}, Position {x = 2, y = 2}]}),
            (Position {x = 1, y = 3}, Point {position = Position {x = 1, y = 3}, value = 7, neighbors = [Position {x = 0, y = 3}, Position {x = 1, y = 2}, Position {x = 1, y = 4}, Position {x = 2, y = 3}]}),
            (Position {x = 1, y = 4}, Point {position = Position {x = 1, y = 4}, value = 8, neighbors = [Position {x = 0, y = 4}, Position {x = 1, y = 3}, Position {x = 2, y = 4}]}),
            (Position {x = 2, y = 0}, Point {position = Position {x = 2, y = 0}, value = 9, neighbors = [Position {x = 1, y = 0}, Position {x = 2, y = 1}, Position {x = 3, y = 0}]}),
            (Position {x = 2, y = 1}, Point {position = Position {x = 2, y = 1}, value = 8, neighbors = [Position {x = 1, y = 1}, Position {x = 2, y = 0}, Position {x = 2, y = 2}, Position {x = 3, y = 1}]}),
            (Position {x = 2, y = 2}, Point {position = Position {x = 2, y = 2}, value = 5, neighbors = [Position {x = 1, y = 2}, Position {x = 2, y = 1}, Position {x = 2, y = 3}, Position {x = 3, y = 2}]}),
            (Position {x = 2, y = 3}, Point {position = Position {x = 2, y = 3}, value = 6, neighbors = [Position {x = 1, y = 3}, Position {x = 2, y = 2}, Position {x = 2, y = 4}, Position {x = 3, y = 3}]}),
            (Position {x = 2, y = 4}, Point {position = Position {x = 2, y = 4}, value = 9, neighbors = [Position {x = 1, y = 4}, Position {x = 2, y = 3}, Position {x = 3, y = 4}]}),
            (Position {x = 3, y = 0}, Point {position = Position {x = 3, y = 0}, value = 9, neighbors = [Position {x = 2, y = 0}, Position {x = 3, y = 1}, Position {x = 4, y = 0}]}),
            (Position {x = 3, y = 1}, Point {position = Position {x = 3, y = 1}, value = 7, neighbors = [Position {x = 2, y = 1}, Position {x = 3, y = 0}, Position {x = 3, y = 2}, Position {x = 4, y = 1}]}),
            (Position {x = 3, y = 2}, Point {position = Position {x = 3, y = 2}, value = 6, neighbors = [Position {x = 2, y = 2}, Position {x = 3, y = 1}, Position {x = 3, y = 3}, Position {x = 4, y = 2}]}),
            (Position {x = 3, y = 3}, Point {position = Position {x = 3, y = 3}, value = 7, neighbors = [Position {x = 2, y = 3}, Position {x = 3, y = 2}, Position {x = 3, y = 4}, Position {x = 4, y = 3}]}),
            (Position {x = 3, y = 4}, Point {position = Position {x = 3, y = 4}, value = 9, neighbors = [Position {x = 2, y = 4}, Position {x = 3, y = 3}, Position {x = 4, y = 4}]}),
            (Position {x = 4, y = 0}, Point {position = Position {x = 4, y = 0}, value = 9, neighbors = [Position {x = 3, y = 0}, Position {x = 4, y = 1}, Position {x = 5, y = 0}]}),
            (Position {x = 4, y = 1}, Point {position = Position {x = 4, y = 1}, value = 8, neighbors = [Position {x = 3, y = 1}, Position {x = 4, y = 0}, Position {x = 4, y = 2}, Position {x = 5, y = 1}]}),
            (Position {x = 4, y = 2}, Point {position = Position {x = 4, y = 2}, value = 7, neighbors = [Position {x = 3, y = 2}, Position {x = 4, y = 1}, Position {x = 4, y = 3}, Position {x = 5, y = 2}]}),
            (Position {x = 4, y = 3}, Point {position = Position {x = 4, y = 3}, value = 8, neighbors = [Position {x = 3, y = 3}, Position {x = 4, y = 2}, Position {x = 4, y = 4}, Position {x = 5, y = 3}]}),
            (Position {x = 4, y = 4}, Point {position = Position {x = 4, y = 4}, value = 9, neighbors = [Position {x = 3, y = 4}, Position {x = 4, y = 3}, Position {x = 5, y = 4}]}),
            (Position {x = 5, y = 0}, Point {position = Position {x = 5, y = 0}, value = 4, neighbors = [Position {x = 4, y = 0}, Position {x = 5, y = 1}, Position {x = 6, y = 0}]}),
            (Position {x = 5, y = 1}, Point {position = Position {x = 5, y = 1}, value = 9, neighbors = [Position {x = 4, y = 1}, Position {x = 5, y = 0}, Position {x = 5, y = 2}, Position {x = 6, y = 1}]}),
            (Position {x = 5, y = 2}, Point {position = Position {x = 5, y = 2}, value = 8, neighbors = [Position {x = 4, y = 2}, Position {x = 5, y = 1}, Position {x = 5, y = 3}, Position {x = 6, y = 2}]}),
            (Position {x = 5, y = 3}, Point {position = Position {x = 5, y = 3}, value = 9, neighbors = [Position {x = 4, y = 3}, Position {x = 5, y = 2}, Position {x = 5, y = 4}, Position {x = 6, y = 3}]}),
            (Position {x = 5, y = 4}, Point {position = Position {x = 5, y = 4}, value = 6, neighbors = [Position {x = 4, y = 4}, Position {x = 5, y = 3}, Position {x = 6, y = 4}]}),
            (Position {x = 6, y = 0}, Point {position = Position {x = 6, y = 0}, value = 3, neighbors = [Position {x = 5, y = 0}, Position {x = 6, y = 1}, Position {x = 7, y = 0}]}),
            (Position {x = 6, y = 1}, Point {position = Position {x = 6, y = 1}, value = 4, neighbors = [Position {x = 5, y = 1}, Position {x = 6, y = 0}, Position {x = 6, y = 2}, Position {x = 7, y = 1}]}),
            (Position {x = 6, y = 2}, Point {position = Position {x = 6, y = 2}, value = 9, neighbors = [Position {x = 5, y = 2}, Position {x = 6, y = 1}, Position {x = 6, y = 3}, Position {x = 7, y = 2}]}),
            (Position {x = 6, y = 3}, Point {position = Position {x = 6, y = 3}, value = 6, neighbors = [Position {x = 5, y = 3}, Position {x = 6, y = 2}, Position {x = 6, y = 4}, Position {x = 7, y = 3}]}),
            (Position {x = 6, y = 4}, Point {position = Position {x = 6, y = 4}, value = 5, neighbors = [Position {x = 5, y = 4}, Position {x = 6, y = 3}, Position {x = 7, y = 4}]}),
            (Position {x = 7, y = 0}, Point {position = Position {x = 7, y = 0}, value = 2, neighbors = [Position {x = 6, y = 0}, Position {x = 7, y = 1}, Position {x = 8, y = 0}]}),
            (Position {x = 7, y = 1}, Point {position = Position {x = 7, y = 1}, value = 9, neighbors = [Position {x = 6, y = 1}, Position {x = 7, y = 0}, Position {x = 7, y = 2}, Position {x = 8, y = 1}]}),
            (Position {x = 7, y = 2}, Point {position = Position {x = 7, y = 2}, value = 8, neighbors = [Position {x = 6, y = 2}, Position {x = 7, y = 1}, Position {x = 7, y = 3}, Position {x = 8, y = 2}]}),
            (Position {x = 7, y = 3}, Point {position = Position {x = 7, y = 3}, value = 7, neighbors = [Position {x = 6, y = 3}, Position {x = 7, y = 2}, Position {x = 7, y = 4}, Position {x = 8, y = 3}]}),
            (Position {x = 7, y = 4}, Point {position = Position {x = 7, y = 4}, value = 6, neighbors = [Position {x = 6, y = 4}, Position {x = 7, y = 3}, Position {x = 8, y = 4}]}),
            (Position {x = 8, y = 0}, Point {position = Position {x = 8, y = 0}, value = 1, neighbors = [Position {x = 7, y = 0}, Position {x = 8, y = 1}, Position {x = 9, y = 0}]}),
            (Position {x = 8, y = 1}, Point {position = Position {x = 8, y = 1}, value = 2, neighbors = [Position {x = 7, y = 1}, Position {x = 8, y = 0}, Position {x = 8, y = 2}, Position {x = 9, y = 1}]}),
            (Position {x = 8, y = 2}, Point {position = Position {x = 8, y = 2}, value = 9, neighbors = [Position {x = 7, y = 2}, Position {x = 8, y = 1}, Position {x = 8, y = 3}, Position {x = 9, y = 2}]}),
            (Position {x = 8, y = 3}, Point {position = Position {x = 8, y = 3}, value = 8, neighbors = [Position {x = 7, y = 3}, Position {x = 8, y = 2}, Position {x = 8, y = 4}, Position {x = 9, y = 3}]}),
            (Position {x = 8, y = 4}, Point {position = Position {x = 8, y = 4}, value = 7, neighbors = [Position {x = 7, y = 4}, Position {x = 8, y = 3}, Position {x = 9, y = 4}]}),
            (Position {x = 9, y = 0}, Point {position = Position {x = 9, y = 0}, value = 0, neighbors = [Position {x = 8, y = 0}, Position {x = 9, y = 1}]}),
            (Position {x = 9, y = 1}, Point {position = Position {x = 9, y = 1}, value = 1, neighbors = [Position {x = 8, y = 1}, Position {x = 9, y = 0}, Position {x = 9, y = 2}]}),
            (Position {x = 9, y = 2}, Point {position = Position {x = 9, y = 2}, value = 2, neighbors = [Position {x = 8, y = 2}, Position {x = 9, y = 1}, Position {x = 9, y = 3}]}),
            (Position {x = 9, y = 3}, Point {position = Position {x = 9, y = 3}, value = 9, neighbors = [Position {x = 8, y = 3}, Position {x = 9, y = 2}, Position {x = 9, y = 4}]}),
            (Position {x = 9, y = 4}, Point {position = Position {x = 9, y = 4}, value = 8, neighbors = [Position {x = 8, y = 4}, Position {x = 9, y = 3}]})
          ]
    }

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` testField

  it "basinFrom" $ do
    basinFrom testField Position {x = 0, y = 0}
      `shouldBe` Basin
        [ Position {x = 1, y = 0},
          Position {x = 0, y = 1},
          Position {x = 0, y = 0}
        ]

    basinFrom testField Position {x = 9, y = 0}
      `shouldBe` Basin
        [ Position {x = 6, y = 1},
          Position {x = 5, y = 0},
          Position {x = 6, y = 0},
          Position {x = 9, y = 2},
          Position {x = 8, y = 1},
          Position {x = 7, y = 0},
          Position {x = 9, y = 1},
          Position {x = 8, y = 0},
          Position {x = 9, y = 0}
        ]

  it "lowPoints" $ do
    lowPoints
      testField
      `shouldBe` [ Point {position = Position {x = 1, y = 0}, value = 1, neighbors = [Position {x = 0, y = 0}, Position {x = 1, y = 1}, Position {x = 2, y = 0}]},
                   Point {position = Position {x = 2, y = 2}, value = 5, neighbors = [Position {x = 1, y = 2}, Position {x = 2, y = 1}, Position {x = 2, y = 3}, Position {x = 3, y = 2}]},
                   Point {position = Position {x = 6, y = 4}, value = 5, neighbors = [Position {x = 5, y = 4}, Position {x = 6, y = 3}, Position {x = 7, y = 4}]},
                   Point {position = Position {x = 9, y = 0}, value = 0, neighbors = [Position {x = 8, y = 0}, Position {x = 9, y = 1}]}
                 ]

  it "largestBasins" $ do
    largestBasins testInput `shouldBe` [14, 9, 9]

main = hspec spec

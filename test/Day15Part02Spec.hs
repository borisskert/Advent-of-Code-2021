module Day15Part02Spec where

import Day15
  ( Field (Field, points, size),
    Point (Point, distance, neighbors, position, riskLevel),
    Position (Position, x, y),
    Size (Size, height, width),
    lowestTotal,
    parseInput,
    toField,
  )
import Data.Map (fromList)
import Day15Part02
import Test.Hspec

testInput = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"

spec :: Spec
spec = do
  it "extend" $ do
    extend
      [ [3, 2, 1],
        [6, 5, 4]
      ]
      `shouldBe` [ [3, 2, 1, 4, 3, 2, 5, 4, 3, 6, 5, 4, 7, 6, 5],
                   [6, 5, 4, 7, 6, 5, 8, 7, 6, 9, 8, 7, 1, 9, 8],
                   [4, 3, 2, 5, 4, 3, 6, 5, 4, 7, 6, 5, 8, 7, 6],
                   [7, 6, 5, 8, 7, 6, 9, 8, 7, 1, 9, 8, 2, 1, 9],
                   [5, 4, 3, 6, 5, 4, 7, 6, 5, 8, 7, 6, 9, 8, 7],
                   [8, 7, 6, 9, 8, 7, 1, 9, 8, 2, 1, 9, 3, 2, 1],
                   [6, 5, 4, 7, 6, 5, 8, 7, 6, 9, 8, 7, 1, 9, 8],
                   [9, 8, 7, 1, 9, 8, 2, 1, 9, 3, 2, 1, 4, 3, 2],
                   [7, 6, 5, 8, 7, 6, 9, 8, 7, 1, 9, 8, 2, 1, 9],
                   [1, 9, 8, 2, 1, 9, 3, 2, 1, 4, 3, 2, 5, 4, 3]
                 ]

  it "initDistance" $ do
    initDistance
      ( Field
          { size = Size {width = 3, height = 2},
            points =
              fromList
                [ 
                    (Position {x = 0, y = 0},Point
                                             { position = Position {x = 0, y = 0},
                                               riskLevel = 1,
                                               neighbors =
                                                 [ Position {x = 0, y = 1},
                                                   Position {x = 1, y = 0}
                                                 ],
                                               distance = 0
                                             }),
                  ( Position {x = 1, y = 0},
                    Point
                      { position = Position {x = 1, y = 0},
                        riskLevel = 2,
                        neighbors =
                          [ Position {x = 0, y = 0},
                            Position {x = 1, y = 1},
                            Position {x = 2, y = 0}
                          ],
                        distance = 0
                      }
                  ),
                  ( Position {x = 2, y = 0},
                    Point
                      { position = Position {x = 2, y = 0},
                        riskLevel = 3,
                        neighbors =
                          [ Position {x = 1, y = 0},
                            Position {x = 2, y = 1}
                          ],
                        distance = 0
                      }
                  ),
                  ( Position {x = 0, y = 1},
                    Point
                      { position = Position {x = 0, y = 1},
                        riskLevel = 4,
                        neighbors =
                          [ Position {x = 0, y = 0},
                            Position {x = 1, y = 1}
                          ],
                        distance = 0
                      }
                  ),
                  ( Position {x = 1, y = 1},
                    Point
                      { position = Position {x = 1, y = 1},
                        riskLevel = 5,
                        neighbors =
                          [ Position {x = 0, y = 1},
                            Position {x = 1, y = 0},
                            Position {x = 2, y = 1}
                          ],
                        distance = 0
                      }
                  ),
                  ( Position {x = 2, y = 1},
                    Point
                      { position = Position {x = 2, y = 1},
                        riskLevel = 6,
                        neighbors =
                          [ Position {x = 1, y = 1},
                            Position {x = 2, y = 0}
                          ],
                        distance = 0
                      }
                  )
                ]
          }
      )
      `shouldBe` ( Field
                     { size = Size {width = 3, height = 2},
                       points =
                         fromList
                           [
                                ( Position {x = 0, y = 0},
                               Point
                                 { position = Position {x = 0, y = 0},
                                   riskLevel = 1,
                                   neighbors =
                                     [ Position {x = 0, y = 1},
                                       Position {x = 1, y = 0}
                                     ],
                                   distance = 0
                                 }
                             ),
                             ( Position {x = 1, y = 0},
                               Point
                                 { position = Position {x = 1, y = 0},
                                   riskLevel = 2,
                                   neighbors =
                                     [ Position {x = 0, y = 0},
                                       Position {x = 1, y = 1},
                                       Position {x = 2, y = 0}
                                     ],
                                   distance = 2
                                 }
                             ),
                             ( Position {x = 2, y = 0},
                               Point
                                 { position = Position {x = 2, y = 0},
                                   riskLevel = 3,
                                   neighbors =
                                     [ Position {x = 1, y = 0},
                                       Position {x = 2, y = 1}
                                     ],
                                   distance = 5
                                 }
                             ),
                             ( Position {x = 0, y = 1},
                               Point
                                 { position = Position {x = 0, y = 1},
                                   riskLevel = 4,
                                   neighbors =
                                     [ Position {x = 0, y = 0},
                                       Position {x = 1, y = 1}
                                     ],
                                   distance = 4
                                 }
                             ),
                             ( Position {x = 1, y = 1},
                               Point
                                 { position = Position {x = 1, y = 1},
                                   riskLevel = 5,
                                   neighbors =
                                     [ Position {x = 0, y = 1},
                                       Position {x = 1, y = 0},
                                       Position {x = 2, y = 1}
                                     ],
                                   distance = 7
                                 }
                             ),
                             ( Position {x = 2, y = 1},
                               Point
                                 { position = Position {x = 2, y = 1},
                                   riskLevel = 6,
                                   neighbors =
                                     [ Position {x = 1, y = 1},
                                       Position {x = 2, y = 0}
                                     ],
                                   distance = 11
                                 }
                             )
                           ]
                     }
                 )

  it "lowestTotal" $ do
    lowestTotalExtended "123\n456\n789" `shouldBe` 97
    lowestTotalExtended "1234\n5678\n9012\n3456" `shouldBe` 114
    lowestTotalExtended "12345\n67890\n12345\n67890\n12345" `shouldBe` 157
    lowestTotalExtended "123456\n789012\n345678\n901234\n567890\n123456" `shouldBe` 196
    lowestTotalExtended "1234567\n8901234\n5678901\n2345678\n9012345\n6789012\n3456789" `shouldBe` 195
    lowestTotalExtended "12345678\n90123456\n78901234\n56789012\n34567890\n12345678\n90123456\n78901234" `shouldBe` 179
    lowestTotalExtended "123456789\n012345678\n901234567\n890123456\n789012345\n678901234\n567890123\n456789012\n345678901" `shouldBe` 151
    lowestTotalExtended testInput `shouldBe` 315

main = hspec spec

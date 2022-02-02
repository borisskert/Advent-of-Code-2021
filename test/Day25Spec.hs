module Day25Spec where

import Data.Map (fromList)
import Day25
import Test.Hspec

simpleTestInput = "...>...\n.......\n......>\nv.....>\n......>\n.......\n..vvv.."

simpleTestInput4 = ">......\n..v....\n..>.v..\n.>.v...\n...>...\n.......\nv......"

simpleCucumbers =
  ( (7, 7),
    fromList
      [ ((0, 3), 'v'),
        ((2, 6), 'v'),
        ((3, 0), '>'),
        ((3, 6), 'v'),
        ((4, 6), 'v'),
        ((6, 2), '>'),
        ((6, 3), '>'),
        ((6, 4), '>')
      ]
  )

testInput = "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"

testInputStep1 = "....>.>v.>\nv.v>.>v.v.\n>v>>..>v..\n>>v>v>.>.v\n.>v.v...v.\nv>>.>vvv..\n..v...>>..\nvv...>>vv.\n>.v.v..v.v"

testInputStep2 = ">.v.v>>..v\nv.v.>>vv..\n>v>.>.>.v.\n>>v>v.>v>.\n.>..v....v\n.>v>>.v.v.\nv....v>v>.\n.vv..>>v..\nv>.....vv."

testInputStep3 = "v>v.v>.>v.\nv...>>.v.v\n>vv>.>v>..\n>>v>v.>.v>\n..>....v..\n.>.>v>v..v\n..v..v>vv>\nv.v..>>v..\n.v>....v.."

testInputStep4 = "v>..v.>>..\nv.v.>.>.v.\n>vv.>>.v>v\n>>.>..v>.>\n..v>v...v.\n..>>.>vv..\n>.v.vv>v.v\n.....>>vv.\nvvv>...v.."

testInputStep5 = "vv>...>v>.\nv.v.v>.>v.\n>.v.>.>.>v\n>v>.>..v>>\n..v>v.v...\n..>.>>vvv.\n.>...v>v..\n..v.v>>v.v\nv.v.>...v."

testInputStep58 = "..>>v>vv..\n..v.>>vv..\n..>>v>>vv.\n..>>>>>vv.\nv......>vv\nv>v....>>v\nvvv.....>>\n>vv......>\n.>v.vv.v.."

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput simpleTestInput `shouldBe` simpleCucumbers

  it "moveEast" $ do
    moveEast simpleCucumbers
      `shouldBe` ( (7, 7),
                   fromList
                     [ ((0, 2), '>'),
                       ((0, 3), 'v'),
                       ((0, 4), '>'),
                       ((2, 6), 'v'),
                       ((3, 6), 'v'),
                       ((4, 0), '>'),
                       ((4, 6), 'v'),
                       ((6, 3), '>')
                     ]
                 )
  it "moveSouth" $ do
    moveSouth
      ( (7, 7),
        fromList
          [ ((0, 2), '>'),
            ((0, 3), 'v'),
            ((0, 4), '>'),
            ((2, 6), 'v'),
            ((3, 6), 'v'),
            ((4, 0), '>'),
            ((4, 6), 'v'),
            ((6, 3), '>')
          ]
      )
      `shouldBe` ( (7, 7),
                   fromList
                     [ ((0, 3), 'v'),
                       ((2, 0), 'v'),
                       ((4, 0), '>'),
                       ((3, 0), 'v'),
                       ((4, 6), 'v'),
                       ((0, 2), '>'),
                       ((6, 3), '>'),
                       ((0, 4), '>')
                     ]
                 )

  it "step" $ do
    step simpleCucumbers
      `shouldBe` ( (7, 7),
                   fromList
                     [ ((0, 3), 'v'),
                       ((2, 0), 'v'),
                       ((4, 0), '>'),
                       ((3, 0), 'v'),
                       ((4, 6), 'v'),
                       ((0, 2), '>'),
                       ((6, 3), '>'),
                       ((0, 4), '>')
                     ]
                 )
    (step . step . step . step $ simpleCucumbers)
      `shouldBe` ( (7, 7),
                   fromList
                     [ ((0, 6), 'v'),
                       ((2, 1), 'v'),
                       ((0, 0), '>'),
                       ((3, 3), 'v'),
                       ((4, 2), 'v'),
                       ((2, 2), '>'),
                       ((1, 3), '>'),
                       ((3, 4), '>')
                     ]
                 )

  it "toString" $ do
    toString simpleCucumbers `shouldBe` simpleTestInput
    (toString . step . step . step . step $ simpleCucumbers) `shouldBe` simpleTestInput4
    (toString . parseInput $ testInput) `shouldBe` testInput
    (toString . step . parseInput $ testInput) `shouldBe` testInputStep1
    (toString . step . parseInput $ testInputStep1) `shouldBe` testInputStep2
    (toString . step . step . parseInput $ testInput) `shouldBe` testInputStep2
    (toString . step . step . step . parseInput $ testInput) `shouldBe` testInputStep3
    (toString . step . step . step . step . parseInput $ testInput) `shouldBe` testInputStep4
    (toString . step . step . step . step . step . parseInput $ testInput) `shouldBe` testInputStep5

  it "stepsUntilDeadlock" $ do
    stepsUntilDeadlock testInput `shouldBe` 58

  it "do something" $ do
    (toString . fst . steps . parseInput $ testInput) `shouldBe` testInputStep58

main = hspec spec

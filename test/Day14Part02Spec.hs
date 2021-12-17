module Day14Part02Spec where

import Data.Map (Map, fromList, lookup, member)
import Day14 (parseInput)
import Day14Part02
import Test.Hspec

testInput = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"

testRules = makeRules . snd . parseInput $ testInput :: Rules

spec :: Spec
spec = do
  it "makeRules" $ do
    makeRules
      [ ("CH", "B"),
        ("HH", "N"),
        ("CB", "H"),
        ("NH", "C"),
        ("HB", "C"),
        ("HC", "B"),
        ("HN", "C"),
        ("NN", "C"),
        ("BH", "H"),
        ("NC", "B"),
        ("NB", "B"),
        ("BN", "B"),
        ("BB", "N"),
        ("BC", "B"),
        ("CC", "N"),
        ("CN", "C")
      ]
      `shouldBe` Rules
        ( fromList
            [ ( Pair ('C', 'H'),
                'B'
              ),
              ( Pair ('H', 'H'),
                'N'
              ),
              ( Pair ('C', 'B'),
                'H'
              ),
              ( Pair ('N', 'H'),
                'C'
              ),
              ( Pair ('H', 'B'),
                'C'
              ),
              ( Pair ('H', 'C'),
                'B'
              ),
              ( Pair ('H', 'N'),
                'C'
              ),
              ( Pair ('N', 'N'),
                'C'
              ),
              ( Pair ('B', 'H'),
                'H'
              ),
              ( Pair ('N', 'C'),
                'B'
              ),
              ( Pair ('N', 'B'),
                'B'
              ),
              ( Pair ('B', 'N'),
                'B'
              ),
              ( Pair ('B', 'B'),
                'N'
              ),
              ( Pair ('B', 'C'),
                'B'
              ),
              ( Pair ('C', 'C'),
                'N'
              ),
              ( Pair ('C', 'N'),
                'C'
              )
            ]
        )

  it "count" $ do
    count (fromList [(Pair ('A', 'B'), 3), (Pair ('B', 'D'), 2), (Pair ('C', 'A'), 1)]) `shouldBe` fromList [('A', 3), ('B', 2), ('C', 1)]
    count (fromList [(Pair ('A', 'C'), 3), (Pair ('B', 'A'), 2), (Pair ('C', 'D'), 1), (Pair ('A', 'E'), 4)]) `shouldBe` fromList [('A', 7), ('B', 2), ('C', 1)]
    count (fromList [(Pair ('A', 'C'), 3), (Pair ('B', 'A'), 2), (Pair ('C', 'D'), 1), (Pair ('A', 'E'), 4)]) `shouldBe` fromList [('A', 7), ('B', 2), ('C', 1)]

  it "perform" $ do
    perform testRules "NN" 0 `shouldBe` [('N', 2)]
    perform testRules "CH" 0 `shouldBe` [('C', 1), ('H', 1)]
    perform testRules "HH" 0 `shouldBe` [('H', 2)]
    perform testRules "BN" 0 `shouldBe` [('B', 1), ('N', 1)]
    perform testRules "BH" 0 `shouldBe` [('B', 1), ('H', 1)]

    perform testRules "NN" 1 `shouldBe` [('C', 1), ('N', 2)]
    perform testRules "CH" 1 `shouldBe` [('B', 1), ('C', 1), ('H', 1)]
    perform testRules "HH" 1 `shouldBe` [('H', 2), ('N', 1)]
    perform testRules "BN" 1 `shouldBe` [('B', 2), ('N', 1)]
    perform testRules "BH" 1 `shouldBe` [('B', 1), ('H', 2)]

    perform testRules "NNCB" 1 `shouldBe` [('B', 2), ('C', 2), ('H', 1), ('N', 2)] -- NCNBCHB
    perform testRules "NNCB" 2 `shouldBe` [('B',6),('C',4),('H',1),('N',2)] -- NBCCNBBBCBHCB
    perform testRules "NNCB" 3 `shouldBe` [('B', 11), ('C', 5), ('H', 4), ('N', 5)] -- NBBBCNCCNBBNBNBBCHBHHBCHB
    perform testRules "NNCB" 10 `shouldBe` [('B', 1749), ('C', 298), ('H', 161), ('N', 865)]
    perform testRules "NNCB" 40 `shouldBe` [('B',2192039569602),('C',6597635301),('H',3849876073),('N',1096047802353)]

  it "polymerElementsExtended" $ do
    polymerElementsExtended testInput 40 `shouldBe` (2192039569602,3849876073)

main = hspec spec

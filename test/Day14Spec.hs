module Day14Spec where

import Data.Map (Map, fromList)
import Day14
import Test.Hspec
import Day14 (toPairs)

testInput = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` ( "NNCB",
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
                 )

  it "replacementRules" $ do
    replacementRules
      [ ("CH", "B"),
        ("HH", "N"),
        ("CB", "H")
      ]
      `shouldBe` fromList
        [ ("CH", "B"),
          ("HH", "N"),
          ("CB", "H")
        ]

  it "toPairs" $ do
    toPairs "ABC" `shouldBe` ["AB", "BC"]

  it "step" $ do
    step
      "NNCB"
      ( replacementRules
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
      )
      `shouldBe` "NCNBCHB"
    step
      "NCNBCHB"
      ( replacementRules
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
      )
      `shouldBe` "NBCCNBBBCBHCB"
    step
      "NBCCNBBBCBHCB"
      ( replacementRules
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
      )
      `shouldBe` "NBBBCNCCNBBNBNBBCHBHHBCHB"
    step
      "NBBBCNCCNBBNBNBBCHBHHBCHB"
      ( replacementRules
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
      )
      `shouldBe` "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"

  it "steps" $ do
    steps
      4
      "NNCB"
      ( replacementRules
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
      )
      `shouldBe` "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
    ( length . steps 10 "NNCB" . replacementRules $
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
      )
      `shouldBe` 3073

  it "polymerElements" $ do
    polymerElements testInput `shouldBe` (1749, 161)

main = hspec spec

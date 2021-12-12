module Day12Spec where

import Day12
import Day12 (Connection (Connection))
import Test.Hspec

testInput = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"

largerTestInput = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"

evenLargerTestInput = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` [ ("start", "A"),
                   ("start", "b"),
                   ("A", "c"),
                   ("A", "b"),
                   ("b", "d"),
                   ("A", "end"),
                   ("b", "end")
                 ]

  it "toCaves" $ do
    toCaves
      [ ("start", "A"),
        ("start", "b"),
        ("A", "c"),
        ("A", "b"),
        ("b", "d"),
        ("A", "end"),
        ("b", "end")
      ]
      `shouldBe` [Cave "start", Cave "A", Cave "b", Cave "c", Cave "d", Cave "end"]

  it "isFinished" $ do
    isFinished (Path [Cave "start"]) `shouldBe` False
    isFinished (Path [Cave "start", Cave "end"]) `shouldBe` True

  it "toConnections" $ do
    toConnections
      [ ("start", "A"),
        ("start", "b"),
        ("A", "c"),
        ("A", "b"),
        ("b", "d"),
        ("A", "end"),
        ("b", "end")
      ]
      `shouldBe` [ Connection (Cave "start", Cave "A"),
                   Connection (Cave "start", Cave "b"),
                   Connection (Cave "A", Cave "c"),
                   Connection (Cave "c", Cave "A"),
                   Connection (Cave "A", Cave "b"),
                   Connection (Cave "b", Cave "A"),
                   Connection (Cave "A", Cave "end"),
                   Connection (Cave "b", Cave "end")
                 ]

    (toConnections . parseInput $ largerTestInput)
      `shouldBe` [ Connection (Cave "dc", Cave "end"),
                   Connection (Cave "start", Cave "HN"),
                   Connection (Cave "start", Cave "kj"),
                   Connection (Cave "start", Cave "dc"),
                   Connection (Cave "dc", Cave "HN"),
                   Connection (Cave "HN", Cave "dc"),
                   Connection (Cave "LN", Cave "dc"),
                   Connection (Cave "dc", Cave "LN"),
                   Connection (Cave "HN", Cave "end"),
                   Connection (Cave "kj", Cave "HN"),
                   Connection (Cave "HN", Cave "kj"),
                   Connection (Cave "kj", Cave "dc"),
                   Connection (Cave "dc", Cave "kj")
                 ]

  it "findPaths" $ do
    findPaths
      [ ("start", "A"),
        ("start", "b"),
        ("A", "c"),
        ("A", "b"),
        ("b", "d"),
        ("A", "end"),
        ("b", "end")
      ]
      `shouldBe` [ Path [Cave "start", Cave "A", Cave "c", Cave "A", Cave "b", Cave "A", Cave "end"],
                   Path [Cave "start", Cave "A", Cave "c", Cave "A", Cave "b", Cave "end"],
                   Path [Cave "start", Cave "A", Cave "c", Cave "A", Cave "end"],
                   Path [Cave "start", Cave "A", Cave "b", Cave "A", Cave "c", Cave "A", Cave "end"],
                   Path [Cave "start", Cave "A", Cave "b", Cave "A", Cave "end"],
                   Path [Cave "start", Cave "A", Cave "b", Cave "end"],
                   Path [Cave "start", Cave "A", Cave "end"],
                   Path [Cave "start", Cave "b", Cave "A", Cave "c", Cave "A", Cave "end"],
                   Path [Cave "start", Cave "b", Cave "A", Cave "end"],
                   Path [Cave "start", Cave "b", Cave "end"]
                 ]

  it "howManyPaths" $ do
    howManyPaths testInput `shouldBe` 10
    howManyPaths largerTestInput `shouldBe` 19
    howManyPaths evenLargerTestInput `shouldBe` 226

main = hspec spec

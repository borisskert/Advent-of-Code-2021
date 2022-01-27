module Day22Spec where

import Day22
import Test.Hspec

simpleTestInput = "on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff x=9..11,y=9..11,z=9..11\non x=10..10,y=10..10,z=10..10"

testInput = "on x=-20..26,y=-36..17,z=-47..7\non x=-20..33,y=-21..23,z=-26..28\non x=-22..28,y=-29..23,z=-38..16\non x=-46..7,y=-6..46,z=-50..-1\non x=-49..1,y=-3..46,z=-24..28\non x=2..47,y=-22..22,z=-23..27\non x=-27..23,y=-28..26,z=-21..29\non x=-39..5,y=-6..47,z=-3..44\non x=-30..21,y=-8..43,z=-13..34\non x=-22..26,y=-27..20,z=-29..19\noff x=-48..-32,y=26..41,z=-47..-37\non x=-12..35,y=6..50,z=-50..-2\noff x=-48..-32,y=-32..-16,z=-15..-5\non x=-18..26,y=-33..15,z=-7..46\noff x=-40..-22,y=-38..-28,z=23..41\non x=-16..35,y=-41..10,z=-47..6\noff x=-32..-23,y=11..30,z=-14..3\non x=-49..-5,y=-3..45,z=-29..18\noff x=18..30,y=-20..-8,z=-3..13\non x=-41..9,y=-7..43,z=-33..15\non x=-54112..-39298,y=-85059..-49293,z=-27449..7877\non x=967..23432,y=45373..81175,z=27513..53682"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput simpleTestInput
      `shouldBe` [ ("on", ((10, 12), (10, 12), (10, 12))),
                   ("on", ((11, 13), (11, 13), (11, 13))),
                   ("off", ((9, 11), (9, 11), (9, 11))),
                   ("on", ((10, 10), (10, 10), (10, 10)))
                 ]

  it "intersection" $ do
    intersection ((2, 2), (2, 2), (2, 2)) ((1, 3), (1, 3), (1, 3)) `shouldBe` Just ((2, 2), (2, 2), (2, 2))
    intersection ((2, 2), (2, 2), (2, 2)) ((1, 2), (1, 2), (1, 2)) `shouldBe` Just ((2, 2), (2, 2), (2, 2))
    intersection ((1, 2), (1, 2), (1, 2)) ((2, 3), (2, 3), (2, 3)) `shouldBe` Just ((2, 2), (2, 2), (2, 2))
    intersection ((0, 0), (0, 0), (0, 0)) ((2, 3), (2, 3), (2, 3)) `shouldBe` Nothing
    intersection ((0, 1), (0, 1), (0, 1)) ((1, 2), (1, 2), (1, 2)) `shouldBe` Just ((1, 1), (1, 1), (1, 1))

  it "howManyAreOn" $ do
    howManyAreOn [("on", ((0, 0), (0, 0), (0, 0))), ("on", ((2, 2), (2, 2), (2, 2)))] `shouldBe` 2
    howManyAreOn [("on", ((0, 0), (0, 0), (0, 0))), ("on", ((1, 1), (1, 1), (1, 1)))] `shouldBe` 2
    howManyAreOn [("on", ((0, 0), (0, 0), (0, 0))), ("on", ((0, 1), (0, 1), (0, 1)))] `shouldBe` 8
    howManyAreOn [("on", ((0, 0), (0, 0), (0, 0))), ("on", ((0, 2), (0, 2), (0, 2)))] `shouldBe` 27
    howManyAreOn [("on", ((0, 0), (0, 0), (0, 0))), ("on", ((1, 2), (1, 2), (1, 2)))] `shouldBe` 9
    howManyAreOn [("on", ((0, 1), (0, 1), (0, 1))), ("on", ((1, 2), (1, 2), (1, 2)))] `shouldBe` 15
    howManyAreOn [("off", ((0, 0), (0, 0), (0, 0))), ("on", ((2, 2), (2, 2), (2, 2)))] `shouldBe` 1
    howManyAreOn [("off", ((1, 1), (1, 1), (1, 1))), ("on", ((2, 2), (2, 2), (2, 2)))] `shouldBe` 1
    howManyAreOn [("off", ((1, 1), (1, 1), (1, 1))), ("on", ((1, 2), (1, 2), (1, 2)))] `shouldBe` 8
    howManyAreOn [("on", ((1, 2), (1, 2), (1, 2))), ("off", ((1, 1), (1, 1), (1, 1)))] `shouldBe` 7
    howManyAreOn [("on", ((1, 2), (1, 2), (1, 2))), ("off", ((1, 1), (1, 1), (1, 1))), ("on", ((2, 3), (2, 3), (2, 3)))] `shouldBe` 14
    howManyAreOn [("on", ((1, 2), (1, 2), (1, 2))), ("off", ((2, 2), (2, 2), (2, 2))), ("on", ((2, 3), (2, 3), (2, 3)))] `shouldBe` 15
    howManyAreOn [("on", ((1, 2), (1, 2), (1, 2))), ("off", ((1, 2), (1, 2), (1, 2)))] `shouldBe` 0
    howManyAreOn [("on", ((1, 2), (1, 2), (1, 2))), ("off", ((2, 3), (2, 3), (2, 4)))] `shouldBe` 7
    howManyAreOn [("on", ((1, 2), (1, 2), (1, 2))), ("off", ((2, 3), (2, 3), (2, 4))), ("on", ((2, 3), (2, 3), (2, 3)))] `shouldBe` 15
    howManyAreOn [("on", ((5, 5), (5, 5), (1, 3))), ("on", ((5, 5), (5, 5), (5, 7))), ("off", ((5, 5), (5, 5), (3, 5)))] `shouldBe` 4
    howManyAreOn [("on", ((1, 2), (1, 2), (1, 1))), ("on", ((1, 1), (1, 2), (1, 2))), ("on", ((1, 2), (1, 1), (1, 2)))] `shouldBe` 7
    howManyAreOn [("on", ((10, 12), (10, 12), (10, 12))), ("on", ((11, 12), (11, 12), (11, 12))), ("on", ((11, 13), (11, 13), (11, 13)))] `shouldBe` 46

  it "onlyWithin" $ do
    onlyWithin ((0, 0), (0, 0), (0, 0)) ("on", ((0, 1), (0, 1), (0, 1))) `shouldBe` Just ("on", ((0, 0), (0, 0), (0, 0)))
    onlyWithin ((1, 1), (1, 1), (1, 1)) ("on", ((0, 2), (0, 2), (0, 2))) `shouldBe` Just ("on", ((1, 1), (1, 1), (1, 1)))

  it "howManyCubesAreOn" $ do
    howManyCubesAreOn testInput `shouldBe` 590784

main = hspec spec

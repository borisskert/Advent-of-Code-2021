module Day17Spec where

import Day17
import Test.Hspec

testInput = "target area: x=20..30, y=-10..-5"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` ((20, 30), (-10, -5))

  it "targetArea" $ do
    targetArea ((20, 30), (-10, -5))
      `shouldBe` (TargetArea (Position (20, -5), Position (30, -10)))

  it "isWithin" $ do
    isWithin
      (Position (20, -5))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` True
    isWithin
      (Position (30, -10))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` True
    isWithin
      (Position (30, -5))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` True
    isWithin
      (Position (20, -10))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` True
    isWithin
      (Position (25, -7))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` True
    isWithin
      (Position (19, -5))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isWithin
      (Position (19, -10))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isWithin
      (Position (20, -4))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isWithin
      (Position (20, -11))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isWithin
      (Position (31, -5))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isWithin
      (Position (31, -10))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isWithin
      (Position (30, -4))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isWithin
      (Position (30, -11))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False

  it "isBeyond" $ do
    isBeyond
      (Position (20, -5))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isBeyond
      (Position (30, -10))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isBeyond
      (Position (30, -5))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isBeyond
      (Position (20, -10))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isBeyond
      (Position (25, -7))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` False
    isBeyond
      (Position (31, -5))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` True
    isBeyond
      (Position (20, -11))
      (TargetArea (Position (20, -5), Position (30, -10)))
      `shouldBe` True

  it "shot" $ do
    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (7, 2))
      `shouldBe` [Position (0, 0), Position (7, 2), Position (13, 3), Position (18, 3), Position (22, 2), Position (25, 0), Position (27, -3), Position (28, -7)]
    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (7, 7))
      `shouldBe` [Position (0,0),Position (7,7),Position (13,13),Position (18,18),Position (22,22),Position (25,25),Position (27,27),Position (28,28),Position (28,28),Position (28,27),Position (28,25),Position (28,22),Position (28,18),Position (28,13),Position (28,7),Position (28,0),Position (28,-8)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (7, 8))
      `shouldBe` [Position (0,0),Position (7,8),Position (13,15),Position (18,21),Position (22,26),Position (25,30),Position (27,33),Position (28,35),Position (28,36),Position (28,36),Position (28,35),Position (28,33),Position (28,30),Position (28,26),Position (28,21),Position (28,15),Position (28,8),Position (28,0),Position (28,-9)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (7, 9))
      `shouldBe` [Position (0,0),Position (7,9),Position (13,17),Position (18,24),Position (22,30),Position (25,35),Position (27,39),Position (28,42),Position (28,44),Position (28,45),Position (28,45),Position (28,44),Position (28,42),Position (28,39),Position (28,35),Position (28,30),Position (28,24),Position (28,17),Position (28,9),Position (28,0),Position (28,-10)]
    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (7, 10))
      `shouldBe` [Position (0,0),Position (7,10),Position (13,19),Position (18,27),Position (22,34),Position (25,40),Position (27,45),Position (28,49),Position (28,52),Position (28,54),Position (28,55),Position (28,55),Position (28,54),Position (28,52),Position (28,49),Position (28,45),Position (28,40),Position (28,34),Position (28,27),Position (28,19),Position (28,10),Position (28,0),Position (28,-11)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (7, 11))
      `shouldBe` [Position (0,0),Position (7,11),Position (13,21),Position (18,30),Position (22,38),Position (25,45),Position (27,51),Position (28,56),Position (28,60),Position (28,63),Position (28,65),Position (28,66),Position (28,66),Position (28,65),Position (28,63),Position (28,60),Position (28,56),Position (28,51),Position (28,45),Position (28,38),Position (28,30),Position (28,21),Position (28,11),Position (28,0),Position (28,-12)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (7, 12))
      `shouldBe` [Position (0,0),Position (7,12),Position (13,23),Position (18,33),Position (22,42),Position (25,50),Position (27,57),Position (28,63),Position (28,68),Position (28,72),Position (28,75),Position (28,77),Position (28,78),Position (28,78),Position (28,77),Position (28,75),Position (28,72),Position (28,68),Position (28,63),Position (28,57),Position (28,50),Position (28,42),Position (28,33),Position (28,23),Position (28,12),Position (28,0),Position (28,-13)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 10))
      `shouldBe` [Position (0,0),Position (6,10),Position (11,19),Position (15,27),Position (18,34),Position (20,40),Position (21,45),Position (21,49),Position (21,52),Position (21,54),Position (21,55),Position (21,55),Position (21,54),Position (21,52),Position (21,49),Position (21,45),Position (21,40),Position (21,34),Position (21,27),Position (21,19),Position (21,10),Position (21,0),Position (21,-11)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 9))
      `shouldBe` [Position (0,0),Position (6,9),Position (11,17),Position (15,24),Position (18,30),Position (20,35),Position (21,39),Position (21,42),Position (21,44),Position (21,45),Position (21,45),Position (21,44),Position (21,42),Position (21,39),Position (21,35),Position (21,30),Position (21,24),Position (21,17),Position (21,9),Position (21,0),Position (21,-10)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 8))
      `shouldBe` [Position (0,0),Position (6,8),Position (11,15),Position (15,21),Position (18,26),Position (20,30),Position (21,33),Position (21,35),Position (21,36),Position (21,36),Position (21,35),Position (21,33),Position (21,30),Position (21,26),Position (21,21),Position (21,15),Position (21,8),Position (21,0),Position (21,-9)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 7))
      `shouldBe` [Position (0,0),Position (6,7),Position (11,13),Position (15,18),Position (18,22),Position (20,25),Position (21,27),Position (21,28),Position (21,28),Position (21,27),Position (21,25),Position (21,22),Position (21,18),Position (21,13),Position (21,7),Position (21,0),Position (21,-8)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 6))
      `shouldBe` [Position (0,0),Position (6,6),Position (11,11),Position (15,15),Position (18,18),Position (20,20),Position (21,21),Position (21,21),Position (21,20),Position (21,18),Position (21,15),Position (21,11),Position (21,6),Position (21,0),Position (21,-7)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 4))
      `shouldBe` [Position (0,0),Position (6,4),Position (11,7),Position (15,9),Position (18,10),Position (20,10),Position (21,9),Position (21,7),Position (21,4),Position (21,0),Position (21,-5)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 3))
      `shouldBe` [Position (0, 0), Position (6, 3), Position (11, 5), Position (15, 6), Position (18, 6), Position (20, 5), Position (21, 3), Position (21, 0), Position (21, -4), Position (21, -9)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 2))
      `shouldBe` [Position (0,0),Position (6,2),Position (11,3),Position (15,3),Position (18,2),Position (20,0),Position (21,-3),Position (21,-7)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (6, 1))
      `shouldBe` [Position (0,0),Position (6,1),Position (11,1),Position (15,0),Position (18,-2),Position (20,-5)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (9, 0))
      `shouldBe` [Position (0, 0), Position (9, 0), Position (17, -1), Position (24, -3), Position (30, -6)]

    shot
      (TargetArea (Position (20, -5), Position (30, -10)))
      (Velocity (17, -4))
      `shouldBe` [Position (0, 0), Position (17, -4), Position (33, -9)]

  it "bestShotMaximumHeight" $ do
    bestShotMaximumHeight testInput `shouldBe` 45  -- Velocity (7,9)
    bestShotMaximumHeight "target area: x=30..40, y=-20..-10" `shouldBe` 190 -- Velocity (8,19)
    bestShotMaximumHeight "target area: x=40..50, y=-30..-20" `shouldBe` 435   -- Velocity (9,29)
    bestShotMaximumHeight "target area: x=50..60, y=-50..-40" `shouldBe` 1225   -- Velocity (10,49)
    bestShotMaximumHeight "target area: x=29..73, y=-248..-194" `shouldBe` 30628 -- Velocity (11,247)

main = hspec spec

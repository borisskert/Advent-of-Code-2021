module Day18Spec where

import Day18
import Day18 (Snailfish (Regular))
import Test.Hspec

testInput = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]"

exampleHomework = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput "[1,2]" `shouldBe` [Pair (Regular 1, Regular 2)]
    parseInput "[[1,2],3]" `shouldBe` [Pair (Pair (Regular 1, Regular 2), Regular 3)]
    parseInput testInput
      `shouldBe` [ Pair (Pair (Pair (Regular 0, Pair (Regular 4, Regular 5)), Pair (Regular 0, Regular 0)), Pair (Pair (Pair (Regular 4, Regular 5), Pair (Regular 2, Regular 6)), Pair (Regular 9, Regular 5))),
                   Pair (Regular 7, Pair (Pair (Pair (Regular 3, Regular 7), Pair (Regular 4, Regular 3)), Pair (Pair (Regular 6, Regular 3), Pair (Regular 8, Regular 8)))),
                   Pair (Pair (Regular 2, Pair (Pair (Regular 0, Regular 8), Pair (Regular 3, Regular 4))), Pair (Pair (Pair (Regular 6, Regular 7), Regular 1), Pair (Regular 7, Pair (Regular 1, Regular 6)))),
                   Pair (Pair (Pair (Pair (Regular 2, Regular 4), Regular 7), Pair (Regular 6, Pair (Regular 0, Regular 5))), Pair (Pair (Pair (Regular 6, Regular 8), Pair (Regular 2, Regular 8)), Pair (Pair (Regular 2, Regular 1), Pair (Regular 4, Regular 5)))),
                   Pair (Regular 7, Pair (Regular 5, Pair (Pair (Regular 3, Regular 8), Pair (Regular 1, Regular 4)))),
                   Pair (Pair (Regular 2, Pair (Regular 2, Regular 2)), Pair (Regular 8, Pair (Regular 8, Regular 1))),
                   Pair (Regular 2, Regular 9),
                   Pair (Regular 1, Pair (Pair (Pair (Regular 9, Regular 3), Regular 9), Pair (Pair (Regular 9, Regular 0), Pair (Regular 0, Regular 7)))),
                   Pair (Pair (Pair (Regular 5, Pair (Regular 7, Regular 4)), Regular 7), Regular 1),
                   Pair (Pair (Pair (Pair (Regular 4, Regular 2), Regular 2), Regular 6), Pair (Regular 8, Regular 7))
                 ]

  it "toString" $ do
    toString (Pair (Regular 1, Regular 2)) `shouldBe` "[1,2]"

  it "explode" $ do
    test explode "[[[[[6,5],4],3],2],1]" `shouldBe` "[[[[0,9],3],2],1]" -- LLLL
    test explode "[[[[4,[6,5]],3],2],1]" `shouldBe` "[[[[10,0],8],2],1]" -- LLLR
    test explode "[[[3,[[6,5],4]],2],1]" `shouldBe` "[[[9,[0,9]],2],1]" -- LLRL
    test explode "[[[3,[4,[6,5]]],2],1]" `shouldBe` "[[[3,[10,0]],7],1]" -- LLRR
    test explode "[[2,[[[6,5],4],3]],1]" `shouldBe` "[[8,[[0,9],3]],1]" -- LRLL
    test explode "[[2,[[4,[6,5]],3]],1]" `shouldBe` "[[2,[[10,0],8]],1]" -- LRLR
    test explode "[[2,[3,[[6,5],4]]],1]" `shouldBe` "[[2,[9,[0,9]]],1]" -- LRRL
    test explode "[[2,[3,[4,[6,5]]]],1]" `shouldBe` "[[2,[3,[10,0]]],6]" -- LRRR
    test explode "[1,[[[[6,5],4],3],2]]" `shouldBe` "[7,[[[0,9],3],2]]" -- RLLL
    test explode "[1,[[[4,[6,5]],3],2]]" `shouldBe` "[1,[[[10,0],8],2]]" -- RLLR
    test explode "[1,[[3,[[6,5],4]],2]]" `shouldBe` "[1,[[9,[0,9]],2]]" -- RLRL
    test explode "[1,[[3,[4,[6,5]]],2]]" `shouldBe` "[1,[[3,[10,0]],7]]" -- RLRR
    test explode "[1,[2,[[[6,5],4],3]]]" `shouldBe` "[1,[8,[[0,9],3]]]" -- RRLL
    test explode "[1,[2,[[4,[6,5]],3]]]" `shouldBe` "[1,[2,[[10,0],8]]]" -- RRLR
    test explode "[1,[2,[3,[[6,5],4]]]]" `shouldBe` "[1,[2,[9,[0,9]]]]" -- RRRL
    test explode "[1,[2,[3,[4,[6,5]]]]]" `shouldBe` "[1,[2,[3,[10,0]]]]" -- RRRR
  it "reduce" $ do
    reduce (Pair (Regular 1, Regular 2)) `shouldBe` Pair (Regular 1, Regular 2)
    test reduce "[[[[[9,8],1],2],3],4]" `shouldBe` "[[[[0,9],2],3],4]"
    test reduce "[7,[6,[5,[4,[3,2]]]]]" `shouldBe` "[7,[6,[5,[7,0]]]]"
    test reduce "[[6,[5,[4,[3,2]]]],1]" `shouldBe` "[[6,[5,[7,0]]],3]"
    test reduce "[[[6,[5,[3,2]]],4],5]" `shouldBe` "[[[6,[8,0]],6],5]"
    test reduce "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" `shouldBe` "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
    test reduce "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]" `shouldBe` "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    test reduce "[[[[[1,1],[2,2]],[3,3]],[4,4]],[5,5]]" `shouldBe` "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    test reduce "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]" `shouldBe` "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    test reduce "[[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]],[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]]" `shouldBe` "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"

  it "add" $ do
    testAdd "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]" `shouldBe` "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    testAdd "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]" `shouldBe` "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
    testAdd "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]" `shouldBe` "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
    testAdd "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]" "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]" `shouldBe` "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"

  it "snailfishSum" $ do
    (toString . snailfishSum $ "[1,1]\n[2,2]\n[3,3]\n[4,4]") `shouldBe` "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    (toString . snailfishSum $ "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]") `shouldBe` "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    (toString . snailfishSum $ "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]") `shouldBe` "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    (toString . snailfishSum $ exampleHomework) `shouldBe` "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"

  it "magnitude" $ do
    (magnitude . slrParse $ "[[1,2],[[3,4],5]]") `shouldBe` 143
    (magnitude . slrParse $ "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") `shouldBe` 1384
    (magnitude . slrParse $ "[[[[1,1],[2,2]],[3,3]],[4,4]]") `shouldBe` 445
    (magnitude . slrParse $ "[[[[3,0],[5,3]],[4,4]],[5,5]]") `shouldBe` 791
    (magnitude . slrParse $ "[[[[5,0],[7,4]],[5,5]],[6,6]]") `shouldBe` 1137
    (magnitude . slrParse $ "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") `shouldBe` 3488
    (magnitude . slrParse $ "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]") `shouldBe` 4140

  it "finalSnailfishSum" $ do
    finalSnailfishSum exampleHomework `shouldBe` 4140

main = hspec spec

test :: (Snailfish -> Snailfish) -> String -> String
test function = toString . function . slrParse

testAdd :: String -> String -> String
testAdd a b = toString $ add parsedA parsedB
  where
    parsedA = slrParse a :: Snailfish
    parsedB = slrParse b :: Snailfish

module Day10Part02Spec where

import Day10Part02
import Test.Hspec

testInput = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

spec :: Spec
spec = do
  it "autocomplete" $ do
    autocomplete "[({(<(())[]>[[{[]{<()<>>" `shouldBe` "}}]])})]"
    autocomplete "[(()[<>])]({[<{<<[]>>(" `shouldBe` ")}>]})"
    autocomplete "(((({<>}<{<{<>}{[]{[]{}" `shouldBe` "}}>}>))))"
    autocomplete "{<[[]]>}<{[{[{[]{()[[[]" `shouldBe` "]]}}]}]}>"
    autocomplete "<{([{{}}[<[[[<>{}]]]>[]]" `shouldBe` "])}>"

  it "toScore" $ do
    toScore "}}]])})]" `shouldBe` 288957
    toScore ")}>]})" `shouldBe` 5566
    toScore "}}>}>))))" `shouldBe` 1480781
    toScore "]]}}]}]}>" `shouldBe` 995444
    toScore "])}>" `shouldBe` 294

  it "middleScore" $ do
    middleScore testInput `shouldBe` 288957

main = hspec spec

module Day10Spec where

import Day10
import Test.Hspec

testInput = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` [ "[({(<(())[]>[[{[]{<()<>>",
                   "[(()[<>])]({[<{<<[]>>(",
                   "{([(<{}[<>[]}>{[]{[(<()>",
                   "(((({<>}<{<{<>}{[]{[]{}",
                   "[[<[([]))<([[{}[[()]]]",
                   "[{[{({}]{}}([{[{{{}}([]",
                   "{<[[]]>}<{[{[{[]{()[[[]",
                   "[<(<(<(<{}))><([]([]()",
                   "<{([([[(<>()){}]>(<<{{",
                   "<{([{{}}[<[[[<>{}]]]>[]]"
                 ]

  it "checkForError" $ do
    checkForError "()" `shouldBe` Nothing
    checkForError "{}" `shouldBe` Nothing
    checkForError "[]" `shouldBe` Nothing
    checkForError "<>" `shouldBe` Nothing
    checkForError "<()>" `shouldBe` Nothing
    checkForError "<({})>" `shouldBe` Nothing
    checkForError "<({[]})>" `shouldBe` Nothing
    checkForError "<({[][]})>" `shouldBe` Nothing
    checkForError "<({[][()()]})>" `shouldBe` Nothing
    checkForError "(])" `shouldBe` Just ']'
    checkForError "()]" `shouldBe` Just ']'
    checkForError "]()" `shouldBe` Just ']'
    checkForError "{([(<{}[<>[]}>{[]{[(<()>" `shouldBe` Just '}'
    checkForError "[[<[([]))<([[{}[[()]]]" `shouldBe` Just ')'
    checkForError "[{[{({}]{}}([{[{{{}}([]" `shouldBe` Just ']'
    checkForError "[<(<(<(<{}))><([]([]()" `shouldBe` Just ')'
    checkForError "<{([([[(<>()){}]>(<<{{" `shouldBe` Just '>'

  it "toScore" $ do
    toScore ')' `shouldBe` 3
    toScore ']' `shouldBe` 57
    toScore '}' `shouldBe` 1197
    toScore '>' `shouldBe` 25137

  it "syntaxErrorScore" $ do
    syntaxErrorScore testInput `shouldBe` 26397

main = hspec spec

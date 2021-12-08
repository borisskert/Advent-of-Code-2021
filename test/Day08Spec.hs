module Day08Spec where

import Day08
import Test.Hspec

simpleTestInput = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

testInput = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput simpleTestInput `shouldBe` [(["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"], ["cdfeb", "fcadb", "cdfeb", "cdbaf"])]

  it "determine" $ do
    determine ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"] `shouldBe` (Wireing {a = 'd', b = 'e', c = 'a', d = 'f', e = 'g', f = 'b', g = 'c'})
    determine ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"] `shouldBe` (Wireing {a = 'd', b = 'g', c = 'b', d = 'c', e = 'a', f = 'e', g = 'f'})

  it "toDigit" $ do
    toDigit (Wireing {a = 'd', b = 'e', c = 'a', d = 'f', e = 'g', f = 'b', g = 'c'}) "cdfeb" `shouldBe` 5

  it "toDigits" $ do
    toDigits simpleTestInput `shouldBe` [[5, 3, 5, 3]]
    toDigits testInput `shouldBe` [[8, 3, 9, 4], [9, 7, 8, 1], [1, 1, 9, 7], [9, 3, 6, 1], [4, 8, 7, 3], [8, 4, 1, 8], [4, 5, 4, 8], [1, 6, 2, 5], [8, 7, 1, 7], [4, 3, 1, 5]]

  it "digitsAppear" $ do
    digitsAppear [5, 3] simpleTestInput `shouldBe` 4
    digitsAppear [1, 4, 7, 8] testInput `shouldBe` 26

main = hspec spec

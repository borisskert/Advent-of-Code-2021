module Day16Part02Spec where

import Day16Part02
import Test.Hspec
import Prelude hiding (Left, Right)

spec :: Spec
spec = do

  it "evaluate" $ do
    evaluate "C200B40A82" `shouldBe` 3
    evaluate "04005AC33890" `shouldBe` 54
    evaluate "880086C3E88112" `shouldBe` 7
    evaluate "CE00C43D881120" `shouldBe` 9
    evaluate "D8005AC2A8F0" `shouldBe` 1
    evaluate "F600BC2D8F" `shouldBe` 0
    evaluate "9C005AC2F8F0" `shouldBe` 0
    evaluate "9C0141080250320F1802104A08" `shouldBe` 1

main = hspec spec

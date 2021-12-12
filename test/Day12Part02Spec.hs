module Day12Part02Spec where

import Day12Part02
import Test.Hspec

testInput = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"

largerTestInput = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"

evenLargerTestInput = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"

spec :: Spec
spec = do

  it "howManyPathsV2" $ do
    howManyPathsV2 testInput `shouldBe` 36
    howManyPathsV2 largerTestInput `shouldBe` 103
    howManyPathsV2 evenLargerTestInput `shouldBe` 3509

main = hspec spec

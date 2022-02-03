module Day24Part02Spec where

import Day24Part02
import Day24Spec (productionInput, productionProgram)
import Test.Hspec

spec :: Spec
spec = do
  it "hackLowestNumber" $ do
    hackLowestNumber productionInput `shouldBe` "71111591176151"

main = hspec spec

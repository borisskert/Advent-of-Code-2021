module Day16Spec where

import Data.Either (Either (Left, Right))
import Day16
import Test.Hspec
import Prelude hiding (Left, Right)

testInput1 = "8A004A801A8002F478"

testInput2 = "620080001611562C8802118E34"

testInput3 = "C0015000016115A2E0802F182340"

testInput4 = "A0016C880162017C3686B18A3D4780"

spec :: Spec
spec = do
  it "readBitsFromString" $ do
    readBitsFromString "D2FE28" `shouldBe` [True, True, False, True, False, False, True, False, True, True, True, True, True, True, True, False, False, False, True, False, True, False, False, False]

  it "readVersionFromBits" $ do
    readVersionFromBits [True, True, False, True, False, False, True, False, True, True, True, True, True, True, True, False, False, False, True, False, True, False, False, False]
      `shouldBe` Version 6

  it "readGroupsFromBits" $ do
    readGroupsFromBits [True, False, True, True, True, True, True, True, True, False, False, False, True, False, True, False, False, False]
      `shouldBe` [Group (False, 7), Group (False, 14), Group (True, 5)]

  it "groupsToPayload" $ do
    groupsToPayload [Group (False, 7), Group (False, 14), Group (True, 5)]
      `shouldBe` 2021

  it "readLiteral" $ do
    readLiteral "D2FE28"
      `shouldBe` Literal (Version 6, TypeId 4, [Group (False, 7), Group (False, 14), Group (True, 5)])

  it "actualBitSize" $ do
    actualBitSize (Left (Literal (Version 6, TypeId 4, [Group (False, 7), Group (False, 14), Group (True, 5)])))
      `shouldBe` 21
    actualBitSize
      ( Right
          ( Operator
              ( Version 1,
                TypeId 6,
                LengthType 0,
                Left (SubPacketsLength 27),
                [ Left (Literal (Version 6, TypeId 4, [Group (True, 10)])),
                  Left (Literal (Version 2, TypeId 4, [Group (False, 1), Group (True, 4)]))
                ]
              )
          )
      )
      `shouldBe` 49

  it "readPackage" $ do
    readPackage "D2FE28" `shouldBe` Left (Literal (Version 6, TypeId 4, [Group (False, 7), Group (False, 14), Group (True, 5)]))
    readPackage "38006F45291200"
      `shouldBe` Right
        ( Operator
            ( Version 1,
              TypeId 6,
              LengthType 0,
              Left (SubPacketsLength 27),
              [ Left (Literal (Version 6, TypeId 4, [Group (True, 10)])),
                Left (Literal (Version 2, TypeId 4, [Group (False, 1), Group (True, 4)]))
              ]
            )
        )
    readPackage testInput1
      `shouldBe` Right
        ( Operator
            ( Version 4,
              TypeId 2,
              LengthType 1,
              Right (SubPacketsCount 1),
              [ Right
                  ( Operator
                      ( Version 1,
                        TypeId 2,
                        LengthType 1,
                        Right (SubPacketsCount 1),
                        [ Right
                            ( Operator
                                ( Version 5,
                                  TypeId 2,
                                  LengthType 0,
                                  Left (SubPacketsLength 11),
                                  [ Left (Literal (Version 6, TypeId 4, [Group (True, 15)]))
                                  ]
                                )
                            )
                        ]
                      )
                  )
              ]
            )
        )

  it "readPackages" $ do
    readPackages testInput1
      `shouldBe` [ Right
                     ( Operator
                         ( Version 4,
                           TypeId 2,
                           LengthType 1,
                           Right (SubPacketsCount 1),
                           [ Right
                               ( Operator
                                   ( Version 1,
                                     TypeId 2,
                                     LengthType 1,
                                     Right (SubPacketsCount 1),
                                     [ Right
                                         ( Operator
                                             ( Version 5,
                                               TypeId 2,
                                               LengthType 0,
                                               Left (SubPacketsLength 11),
                                               [ Left (Literal (Version 6, TypeId 4, [Group (True, 15)]))
                                               ]
                                             )
                                         )
                                     ]
                                   )
                               )
                           ]
                         )
                     )
                 ]

  it "readPackagesFromBits" $ do
    readPackagesFromBits [True, True, False, True, False, False, False, True, False, True, False, False, True, False, True, False, False, True, False, False, False, True, False, False, True, False, False]
      `shouldBe` [ Left (Literal (Version 6, TypeId 4, [Group (True, 10)])),
                   Left (Literal (Version 2, TypeId 4, [Group (False, 1), Group (True, 4)]))
                 ]

  it "versionSum" $ do
    versionSum testInput1 `shouldBe` 16
    versionSum testInput2 `shouldBe` 12
    versionSum testInput3 `shouldBe` 23
    versionSum testInput4 `shouldBe` 31

main = hspec spec

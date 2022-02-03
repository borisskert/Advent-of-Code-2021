module Day24Spec where

import Data.Map (empty, fromList)
import Day24
import Test.Hspec

verySimpleInput = "inp x\nmul x -1"

verySimpleProgram =
  [ ("inp", [Register 'x']),
    ("mul", [Register 'x', Value (-1)])
  ]

simpleInput = "inp z\ninp x\nmul z 3\neql z x"

simpleProgram =
  [ ("inp", [Register 'z']),
    ("inp", [Register 'x']),
    ("mul", [Register 'z', Value 3]),
    ("eql", [Register 'z', Register 'x'])
  ]

testInput = "inp z\ninp x\nmul z 3\neql z x\ninp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd x w\nmod x 2\ndiv w 2\nmod w 2"

testProgram = [("inp", [Register 'z']), ("inp", [Register 'x']), ("mul", [Register 'z', Value 3]), ("eql", [Register 'z', Register 'x']), ("inp", [Register 'w']), ("add", [Register 'z', Register 'w']), ("mod", [Register 'z', Value 2]), ("div", [Register 'w', Value 2]), ("add", [Register 'y', Register 'w']), ("mod", [Register 'y', Value 2]), ("div", [Register 'w', Value 2]), ("add", [Register 'x', Register 'w']), ("mod", [Register 'x', Value 2]), ("div", [Register 'w', Value 2]), ("mod", [Register 'w', Value 2])]

productionInput = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 13\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 5\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 14\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 15\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 16\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -16\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 8\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -6\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 2\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 13\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 16\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -8\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 6\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -11\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 9\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 12\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 11\nmul y x\nadd z y\ninp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 5\nmul y x\nadd z y\n"

productionProgram = parseInput productionInput

productionInputPart01 = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 13\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 5\nmul y x\nadd z y"

productionInputPart02 = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 14\nmul y x\nadd z y"

productionInputPart03 = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 15\nmul y x\nadd z y"

productionProgramPart01 = parseInput productionInputPart01

productionProgramPart02 = parseInput productionInputPart02

productionProgramPart03 = parseInput productionInputPart03

productionProgramLastPart = "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 26\nadd x -15\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 5\nmul y x\nadd z y"

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput verySimpleInput `shouldBe` verySimpleProgram
    parseInput simpleInput `shouldBe` simpleProgram
    parseInput testInput `shouldBe` testProgram

  it "inp" $ do
    inp [Register 'w'] ("1234", empty) `shouldBe` ("234", fromList [('w', 1)])
    inp [Register 'x'] ("234", fromList [('w', 1)]) `shouldBe` ("34", fromList [('w', 1), ('x', 2)])
    inp [Register 'y'] ("34", fromList [('w', 1), ('x', 2)]) `shouldBe` ("4", fromList [('w', 1), ('x', 2), ('y', 3)])
    inp [Register 'z'] ("4", fromList [('w', 1), ('x', 2), ('y', 3)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)])
    inp [Register 'y'] ("4", fromList [('w', 1), ('x', 2), ('y', 3)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 4)])
    inp [Register 'x'] ("4", fromList [('w', 1), ('x', 2), ('y', 3)]) `shouldBe` ("", fromList [('w', 1), ('x', 4), ('y', 3)])
    inp [Register 'w'] ("4", fromList [('w', 1), ('x', 2), ('y', 3)]) `shouldBe` ("", fromList [('w', 4), ('x', 2), ('y', 3)])

  it "add" $ do
    add [Register 'w', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 7), ('x', 2), ('y', 3), ('z', 4)])
    add [Register 'x', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 8), ('y', 3), ('z', 4)])
    add [Register 'y', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 9), ('z', 4)])
    add [Register 'z', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 10)])
    add [Register 'w', Register 'w'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 2), ('x', 2), ('y', 3), ('z', 4)])
    add [Register 'x', Register 'w'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 3), ('y', 3), ('z', 4)])
    add [Register 'y', Register 'w'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 4), ('z', 4)])
    add [Register 'z', Register 'w'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 5)])
    add [Register 'w', Register 'x'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 3), ('x', 2), ('y', 3), ('z', 4)])
    add [Register 'w', Register 'y'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 4), ('x', 2), ('y', 3), ('z', 4)])
    add [Register 'w', Register 'z'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 5), ('x', 2), ('y', 3), ('z', 4)])

  it "mul" $ do
    mul [Register 'w', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 6), ('x', 2), ('y', 3), ('z', 4)])
    mul [Register 'x', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 12), ('y', 3), ('z', 4)])
    mul [Register 'y', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 18), ('z', 4)])
    mul [Register 'z', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 24)])
    mul [Register 'w', Register 'w'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)])
    mul [Register 'w', Register 'x'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 2), ('x', 2), ('y', 3), ('z', 4)])
    mul [Register 'w', Register 'y'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 3), ('x', 2), ('y', 3), ('z', 4)])
    mul [Register 'w', Register 'z'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 4), ('x', 2), ('y', 3), ('z', 4)])

  it "eql" $ do
    eql [Register 'w', Value 1] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)])
    eql [Register 'w', Value 0] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 0), ('x', 2), ('y', 3), ('z', 4)])
    eql [Register 'w', Value 6] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 0), ('x', 2), ('y', 3), ('z', 4)])
    eql [Register 'w', Register 'x'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 0), ('x', 2), ('y', 3), ('z', 4)])
    eql [Register 'w', Register 'w'] ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)])
    eql [Register 'w', Register 'x'] ("", fromList [('w', 2), ('x', 2), ('y', 3), ('z', 4)]) `shouldBe` ("", fromList [('w', 1), ('x', 2), ('y', 3), ('z', 4)])

  it "process" $ do
    process "3" []
      `shouldBe` fromList
        [ ('x', 0),
          ('y', 0),
          ('z', 0),
          ('w', 0)
        ]
    process "3" verySimpleProgram
      `shouldBe` fromList
        [ ('x', -3),
          ('y', 0),
          ('z', 0),
          ('w', 0)
        ]

    process "999" testProgram `shouldBe` fromList [('w', 1), ('x', 1), ('y', 0), ('z', 1)]
    process "998" testProgram `shouldBe` fromList [('w', 1), ('x', 1), ('y', 0), ('z', 0)]

  it "validationCode" $ do
    validationCode "00000000000000" productionProgram `shouldBe` 65848723
    validationCode "10000000000000" productionProgram `shouldBe` 77730099
    validationCode "11000000000000" productionProgram `shouldBe` 78187075
    validationCode "11100000000000" productionProgram `shouldBe` 78187075
    validationCode "11200000000000" productionProgram `shouldBe` 78187075
    validationCode "12000000000000" productionProgram `shouldBe` 78644051
    validationCode "13000000000000" productionProgram `shouldBe` 79101027
    validationCode "14000000000000" productionProgram `shouldBe` 79558003
    validationCode "15000000000000" productionProgram `shouldBe` 80014979
    validationCode "16000000000000" productionProgram `shouldBe` 80471955
    validationCode "17000000000000" productionProgram `shouldBe` 80928931
    validationCode "18000000000000" productionProgram `shouldBe` 81385907
    validationCode "19000000000000" productionProgram `shouldBe` 81842883
    validationCode "20000000000000" productionProgram `shouldBe` 89611475
    validationCode "30000000000000" productionProgram `shouldBe` 101492851
    validationCode "40000000000000" productionProgram `shouldBe` 113374227
    validationCode "50000000000000" productionProgram `shouldBe` 125255603
    validationCode "60000000000000" productionProgram `shouldBe` 137136979
    validationCode "70000000000000" productionProgram `shouldBe` 149018355
    validationCode "80000000000000" productionProgram `shouldBe` 160899731
    validationCode "90000000000000" productionProgram `shouldBe` 172781107
    validationCode "11111111111111" productionProgram `shouldBe` 78205354
    validationCode "11111111111112" productionProgram `shouldBe` 78205355
    validationCode "11111111111113" productionProgram `shouldBe` 78205356
    validationCode "11111111111114" productionProgram `shouldBe` 78205357
    validationCode "11111111111115" productionProgram `shouldBe` 78205358
    validationCode "11111111111116" productionProgram `shouldBe` 78205359
    validationCode "11111111111117" productionProgram `shouldBe` 78205360
    validationCode "11111111111118" productionProgram `shouldBe` 78205361
    validationCode "11111111111119" productionProgram `shouldBe` 78205362
    validationCode "11111111111121" productionProgram `shouldBe` 78205354
    validationCode "11111111111131" productionProgram `shouldBe` 78205354
    validationCode "11111111111141" productionProgram `shouldBe` 78205354
    validationCode "11111111111151" productionProgram `shouldBe` 3007898
    validationCode "11111111111251" productionProgram `shouldBe` 3007899
    validationCode "11111111111161" productionProgram `shouldBe` 78205354
    validationCode "11111111111171" productionProgram `shouldBe` 78205354
    validationCode "11111111111181" productionProgram `shouldBe` 78205354
    validationCode "11111111111191" productionProgram `shouldBe` 78205354
    validationCode "15111111111111" productionProgram `shouldBe` 80033258
    validationCode "22222222222222" productionProgram `shouldBe` 90561985
    validationCode "33333333333333" productionProgram `shouldBe` 102918616
    validationCode "88888888888888" productionProgram `shouldBe` 164701771
    validationCode "88888888888888" productionProgram `shouldBe` 164701771
    validationCode "99822939611111" productionProgram `shouldBe` 176952730
    validationCode "99822941611111" productionProgram `shouldBe` 176964898
    validationCode "99999999999999" productionProgram `shouldBe` 177058402
    validationCode "99999999999998" productionProgram `shouldBe` 177058401
    validationCode "99999999999997" productionProgram `shouldBe` 177058400
    validationCode "99999999999996" productionProgram `shouldBe` 177058399
    validationCode "99999999999995" productionProgram `shouldBe` 6809938
    validationCode "99999999999985" productionProgram `shouldBe` 177058398
    validationCode "99999999999975" productionProgram `shouldBe` 177058398
    validationCode "99999999999965" productionProgram `shouldBe` 177058398
    validationCode "99999999999955" productionProgram `shouldBe` 177058398
    validationCode "99999999999945" productionProgram `shouldBe` 177058398
    validationCode "99999999999935" productionProgram `shouldBe` 177058398
    validationCode "99999999999925" productionProgram `shouldBe` 177058398
    validationCode "99999999999915" productionProgram `shouldBe` 177058398
    validationCode "99999999999895" productionProgram `shouldBe` 6809937
    validationCode "99999999999795" productionProgram `shouldBe` 6809936
    validationCode "99999999999695" productionProgram `shouldBe` 6809935
    validationCode "99999999999595" productionProgram `shouldBe` 6809934
    validationCode "99999999999495" productionProgram `shouldBe` 261920
    validationCode "99999999989495" productionProgram `shouldBe` 261920

    validationCode "99999999998895" productionProgram `shouldBe` 6809937
    validationCode "99999999997895" productionProgram `shouldBe` 261915
    validationCode "99999999987895" productionProgram `shouldBe` 6809937
    validationCode "99999999977895" productionProgram `shouldBe` 6809937
    validationCode "99999999967895" productionProgram `shouldBe` 6809937
    validationCode "99999999957895" productionProgram `shouldBe` 6809937
    validationCode "99999999947895" productionProgram `shouldBe` 6809937
    validationCode "99999999937895" productionProgram `shouldBe` 6809937
    validationCode "99999999927895" productionProgram `shouldBe` 6809937
    validationCode "99999999917895" productionProgram `shouldBe` 6809937
    validationCode "99999999897895" productionProgram `shouldBe` 261915

    validationCode "99999998897895" productionProgram `shouldBe` 261915
    validationCode "99999998797895" productionProgram `shouldBe` 261915
    validationCode "99999998697895" productionProgram `shouldBe` 261915
    validationCode "99999998597895" productionProgram `shouldBe` 261915
    validationCode "99999998497895" productionProgram `shouldBe` 261915
    validationCode "99999998397895" productionProgram `shouldBe` 261915
    validationCode "99999998297895" productionProgram `shouldBe` 261915
    validationCode "99999998197895" productionProgram `shouldBe` 261915
    validationCode "99999997997895" productionProgram `shouldBe` 261915
    validationCode "99999997897895" productionProgram `shouldBe` 261915

    validationCode "99999999999885" productionProgram `shouldBe` 177058372
    validationCode "99999999999875" productionProgram `shouldBe` 177058372
    validationCode "99999999999865" productionProgram `shouldBe` 177058372

    validationCode "99999999999994" productionProgram `shouldBe` 177058397
    validationCode "99999999999993" productionProgram `shouldBe` 177058396
    validationCode "99999999999992" productionProgram `shouldBe` 177058395
    validationCode "99999999999991" productionProgram `shouldBe` 177058394
    validationCode "99999999999989" productionProgram `shouldBe` 177058402
    validationCode "87977479997495" productionProgram `shouldBe` 9347
    validationCode "87877479997495" productionProgram `shouldBe` 9347
    validationCode "87777479997495" productionProgram `shouldBe` 9347
    validationCode "87677479997495" productionProgram `shouldBe` 9347
    validationCode "87577479997495" productionProgram `shouldBe` 9347

  it "validationCode of optimum" $ do
    validationCode "71599991396195" productionProgram `shouldBe` 0
    validationCode "91599991396195" productionProgram `shouldBe` 10
    validationCode "81599991396195" productionProgram `shouldBe` 10
    validationCode "72599991396195" productionProgram `shouldBe` 322
    validationCode "71699991396195" productionProgram `shouldBe` 8512
    validationCode "71599992396195" productionProgram `shouldBe` 12
    validationCode "71599991496195" productionProgram `shouldBe` 326
    validationCode "71599991397195" productionProgram `shouldBe` 322
    validationCode "71599991396295" productionProgram `shouldBe` 11
    validationCode "71599991396196" productionProgram `shouldBe` 11

  it "numberGroups" $ do
    numberGroups productionProgram
      `shouldBe` [ ["9", "8", "7", "6", "5", "4", "3", "2", "1"],
                   ["9", "8", "7", "6", "5", "4", "3", "2", "1"],
                   ["98764321", "5"],
                   ["9", "87654321"],
                   ["9", "87654321"],
                   ["98754321", "6"],
                   ["9", "8", "7", "6", "5", "4", "3", "2", "1"],
                   ["9", "8", "7", "6", "5", "4", "3", "2", "1"],
                   ["98765421", "3"],
                   ["987654321"],
                   ["98654321", "7"],
                   ["9", "8", "7", "6", "5", "4", "3", "2", "1"],
                   ["987654321"],
                   ["9", "8", "7", "6", "5", "4", "3", "2", "1"]
                 ]

  it "hackLargestNumber" $ do
    hackLargestNumber productionInput `shouldBe` "91599994399395"

main = hspec spec

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

  it "splitIntoSubPrograms" $ do
    splitIntoSubPrograms productionProgram
      `shouldBe` [ [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 1]), ("add", [Register 'x', Value 13]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 5]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 1]), ("add", [Register 'x', Value 15]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 14]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 1]), ("add", [Register 'x', Value 15]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 15]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 1]), ("add", [Register 'x', Value 11]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 16]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 26]), ("add", [Register 'x', Value (-16)]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 8]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 26]), ("add", [Register 'x', Value (-11)]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 9]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 26]), ("add", [Register 'x', Value (-6)]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 2]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 1]), ("add", [Register 'x', Value 11]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 13]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 1]), ("add", [Register 'x', Value 10]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 16]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 26]), ("add", [Register 'x', Value (-10)]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 6]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 26]), ("add", [Register 'x', Value (-8)]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 6]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 26]), ("add", [Register 'x', Value (-11)]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 9]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 1]), ("add", [Register 'x', Value 12]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 11]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])],
                   [("inp", [Register 'w']), ("mul", [Register 'x', Value 0]), ("add", [Register 'x', Register 'z']), ("mod", [Register 'x', Value 26]), ("div", [Register 'z', Value 26]), ("add", [Register 'x', Value (-15)]), ("eql", [Register 'x', Register 'w']), ("eql", [Register 'x', Value 0]), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Value 25]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'y', Value 1]), ("mul", [Register 'z', Register 'y']), ("mul", [Register 'y', Value 0]), ("add", [Register 'y', Register 'w']), ("add", [Register 'y', Value 5]), ("mul", [Register 'y', Register 'x']), ("add", [Register 'z', Register 'y'])]
                 ]

  it "processZ" $ do
    processZ productionProgram "91599" `shouldBe` [14, 379, 9874, 256749, 9874]

  it "allNumbers" $ do
    allNumbers 2 `shouldBe` ["11", "12", "13", "14", "15", "16", "17", "18", "19", "21", "22", "23", "24", "25", "26", "27", "28", "29", "31", "32", "33", "34", "35", "36", "37", "38", "39", "41", "42", "43", "44", "45", "46", "47", "48", "49", "51", "52", "53", "54", "55", "56", "57", "58", "59", "61", "62", "63", "64", "65", "66", "67", "68", "69", "71", "72", "73", "74", "75", "76", "77", "78", "79", "81", "82", "83", "84", "85", "86", "87", "88", "89", "91", "92", "93", "94", "95", "96", "97", "98", "99"]

  xit "findPart01" $ do
    (length . findPart01 $ productionProgram) `shouldBe` 6561

  it "extendNumbers" $ do
    extendNumbers 3 ["11", "23", "44"] `shouldBe` ["11111", "11112", "11113", "11114", "11115", "11116", "11117", "11118", "11119", "11121", "11122", "11123", "11124", "11125", "11126", "11127", "11128", "11129", "11131", "11132", "11133", "11134", "11135", "11136", "11137", "11138", "11139", "11141", "11142", "11143", "11144", "11145", "11146", "11147", "11148", "11149", "11151", "11152", "11153", "11154", "11155", "11156", "11157", "11158", "11159", "11161", "11162", "11163", "11164", "11165", "11166", "11167", "11168", "11169", "11171", "11172", "11173", "11174", "11175", "11176", "11177", "11178", "11179", "11181", "11182", "11183", "11184", "11185", "11186", "11187", "11188", "11189", "11191", "11192", "11193", "11194", "11195", "11196", "11197", "11198", "11199", "11211", "11212", "11213", "11214", "11215", "11216", "11217", "11218", "11219", "11221", "11222", "11223", "11224", "11225", "11226", "11227", "11228", "11229", "11231", "11232", "11233", "11234", "11235", "11236", "11237", "11238", "11239", "11241", "11242", "11243", "11244", "11245", "11246", "11247", "11248", "11249", "11251", "11252", "11253", "11254", "11255", "11256", "11257", "11258", "11259", "11261", "11262", "11263", "11264", "11265", "11266", "11267", "11268", "11269", "11271", "11272", "11273", "11274", "11275", "11276", "11277", "11278", "11279", "11281", "11282", "11283", "11284", "11285", "11286", "11287", "11288", "11289", "11291", "11292", "11293", "11294", "11295", "11296", "11297", "11298", "11299", "11311", "11312", "11313", "11314", "11315", "11316", "11317", "11318", "11319", "11321", "11322", "11323", "11324", "11325", "11326", "11327", "11328", "11329", "11331", "11332", "11333", "11334", "11335", "11336", "11337", "11338", "11339", "11341", "11342", "11343", "11344", "11345", "11346", "11347", "11348", "11349", "11351", "11352", "11353", "11354", "11355", "11356", "11357", "11358", "11359", "11361", "11362", "11363", "11364", "11365", "11366", "11367", "11368", "11369", "11371", "11372", "11373", "11374", "11375", "11376", "11377", "11378", "11379", "11381", "11382", "11383", "11384", "11385", "11386", "11387", "11388", "11389", "11391", "11392", "11393", "11394", "11395", "11396", "11397", "11398", "11399", "11411", "11412", "11413", "11414", "11415", "11416", "11417", "11418", "11419", "11421", "11422", "11423", "11424", "11425", "11426", "11427", "11428", "11429", "11431", "11432", "11433", "11434", "11435", "11436", "11437", "11438", "11439", "11441", "11442", "11443", "11444", "11445", "11446", "11447", "11448", "11449", "11451", "11452", "11453", "11454", "11455", "11456", "11457", "11458", "11459", "11461", "11462", "11463", "11464", "11465", "11466", "11467", "11468", "11469", "11471", "11472", "11473", "11474", "11475", "11476", "11477", "11478", "11479", "11481", "11482", "11483", "11484", "11485", "11486", "11487", "11488", "11489", "11491", "11492", "11493", "11494", "11495", "11496", "11497", "11498", "11499", "11511", "11512", "11513", "11514", "11515", "11516", "11517", "11518", "11519", "11521", "11522", "11523", "11524", "11525", "11526", "11527", "11528", "11529", "11531", "11532", "11533", "11534", "11535", "11536", "11537", "11538", "11539", "11541", "11542", "11543", "11544", "11545", "11546", "11547", "11548", "11549", "11551", "11552", "11553", "11554", "11555", "11556", "11557", "11558", "11559", "11561", "11562", "11563", "11564", "11565", "11566", "11567", "11568", "11569", "11571", "11572", "11573", "11574", "11575", "11576", "11577", "11578", "11579", "11581", "11582", "11583", "11584", "11585", "11586", "11587", "11588", "11589", "11591", "11592", "11593", "11594", "11595", "11596", "11597", "11598", "11599", "11611", "11612", "11613", "11614", "11615", "11616", "11617", "11618", "11619", "11621", "11622", "11623", "11624", "11625", "11626", "11627", "11628", "11629", "11631", "11632", "11633", "11634", "11635", "11636", "11637", "11638", "11639", "11641", "11642", "11643", "11644", "11645", "11646", "11647", "11648", "11649", "11651", "11652", "11653", "11654", "11655", "11656", "11657", "11658", "11659", "11661", "11662", "11663", "11664", "11665", "11666", "11667", "11668", "11669", "11671", "11672", "11673", "11674", "11675", "11676", "11677", "11678", "11679", "11681", "11682", "11683", "11684", "11685", "11686", "11687", "11688", "11689", "11691", "11692", "11693", "11694", "11695", "11696", "11697", "11698", "11699", "11711", "11712", "11713", "11714", "11715", "11716", "11717", "11718", "11719", "11721", "11722", "11723", "11724", "11725", "11726", "11727", "11728", "11729", "11731", "11732", "11733", "11734", "11735", "11736", "11737", "11738", "11739", "11741", "11742", "11743", "11744", "11745", "11746", "11747", "11748", "11749", "11751", "11752", "11753", "11754", "11755", "11756", "11757", "11758", "11759", "11761", "11762", "11763", "11764", "11765", "11766", "11767", "11768", "11769", "11771", "11772", "11773", "11774", "11775", "11776", "11777", "11778", "11779", "11781", "11782", "11783", "11784", "11785", "11786", "11787", "11788", "11789", "11791", "11792", "11793", "11794", "11795", "11796", "11797", "11798", "11799", "11811", "11812", "11813", "11814", "11815", "11816", "11817", "11818", "11819", "11821", "11822", "11823", "11824", "11825", "11826", "11827", "11828", "11829", "11831", "11832", "11833", "11834", "11835", "11836", "11837", "11838", "11839", "11841", "11842", "11843", "11844", "11845", "11846", "11847", "11848", "11849", "11851", "11852", "11853", "11854", "11855", "11856", "11857", "11858", "11859", "11861", "11862", "11863", "11864", "11865", "11866", "11867", "11868", "11869", "11871", "11872", "11873", "11874", "11875", "11876", "11877", "11878", "11879", "11881", "11882", "11883", "11884", "11885", "11886", "11887", "11888", "11889", "11891", "11892", "11893", "11894", "11895", "11896", "11897", "11898", "11899", "11911", "11912", "11913", "11914", "11915", "11916", "11917", "11918", "11919", "11921", "11922", "11923", "11924", "11925", "11926", "11927", "11928", "11929", "11931", "11932", "11933", "11934", "11935", "11936", "11937", "11938", "11939", "11941", "11942", "11943", "11944", "11945", "11946", "11947", "11948", "11949", "11951", "11952", "11953", "11954", "11955", "11956", "11957", "11958", "11959", "11961", "11962", "11963", "11964", "11965", "11966", "11967", "11968", "11969", "11971", "11972", "11973", "11974", "11975", "11976", "11977", "11978", "11979", "11981", "11982", "11983", "11984", "11985", "11986", "11987", "11988", "11989", "11991", "11992", "11993", "11994", "11995", "11996", "11997", "11998", "11999", "23111", "23112", "23113", "23114", "23115", "23116", "23117", "23118", "23119", "23121", "23122", "23123", "23124", "23125", "23126", "23127", "23128", "23129", "23131", "23132", "23133", "23134", "23135", "23136", "23137", "23138", "23139", "23141", "23142", "23143", "23144", "23145", "23146", "23147", "23148", "23149", "23151", "23152", "23153", "23154", "23155", "23156", "23157", "23158", "23159", "23161", "23162", "23163", "23164", "23165", "23166", "23167", "23168", "23169", "23171", "23172", "23173", "23174", "23175", "23176", "23177", "23178", "23179", "23181", "23182", "23183", "23184", "23185", "23186", "23187", "23188", "23189", "23191", "23192", "23193", "23194", "23195", "23196", "23197", "23198", "23199", "23211", "23212", "23213", "23214", "23215", "23216", "23217", "23218", "23219", "23221", "23222", "23223", "23224", "23225", "23226", "23227", "23228", "23229", "23231", "23232", "23233", "23234", "23235", "23236", "23237", "23238", "23239", "23241", "23242", "23243", "23244", "23245", "23246", "23247", "23248", "23249", "23251", "23252", "23253", "23254", "23255", "23256", "23257", "23258", "23259", "23261", "23262", "23263", "23264", "23265", "23266", "23267", "23268", "23269", "23271", "23272", "23273", "23274", "23275", "23276", "23277", "23278", "23279", "23281", "23282", "23283", "23284", "23285", "23286", "23287", "23288", "23289", "23291", "23292", "23293", "23294", "23295", "23296", "23297", "23298", "23299", "23311", "23312", "23313", "23314", "23315", "23316", "23317", "23318", "23319", "23321", "23322", "23323", "23324", "23325", "23326", "23327", "23328", "23329", "23331", "23332", "23333", "23334", "23335", "23336", "23337", "23338", "23339", "23341", "23342", "23343", "23344", "23345", "23346", "23347", "23348", "23349", "23351", "23352", "23353", "23354", "23355", "23356", "23357", "23358", "23359", "23361", "23362", "23363", "23364", "23365", "23366", "23367", "23368", "23369", "23371", "23372", "23373", "23374", "23375", "23376", "23377", "23378", "23379", "23381", "23382", "23383", "23384", "23385", "23386", "23387", "23388", "23389", "23391", "23392", "23393", "23394", "23395", "23396", "23397", "23398", "23399", "23411", "23412", "23413", "23414", "23415", "23416", "23417", "23418", "23419", "23421", "23422", "23423", "23424", "23425", "23426", "23427", "23428", "23429", "23431", "23432", "23433", "23434", "23435", "23436", "23437", "23438", "23439", "23441", "23442", "23443", "23444", "23445", "23446", "23447", "23448", "23449", "23451", "23452", "23453", "23454", "23455", "23456", "23457", "23458", "23459", "23461", "23462", "23463", "23464", "23465", "23466", "23467", "23468", "23469", "23471", "23472", "23473", "23474", "23475", "23476", "23477", "23478", "23479", "23481", "23482", "23483", "23484", "23485", "23486", "23487", "23488", "23489", "23491", "23492", "23493", "23494", "23495", "23496", "23497", "23498", "23499", "23511", "23512", "23513", "23514", "23515", "23516", "23517", "23518", "23519", "23521", "23522", "23523", "23524", "23525", "23526", "23527", "23528", "23529", "23531", "23532", "23533", "23534", "23535", "23536", "23537", "23538", "23539", "23541", "23542", "23543", "23544", "23545", "23546", "23547", "23548", "23549", "23551", "23552", "23553", "23554", "23555", "23556", "23557", "23558", "23559", "23561", "23562", "23563", "23564", "23565", "23566", "23567", "23568", "23569", "23571", "23572", "23573", "23574", "23575", "23576", "23577", "23578", "23579", "23581", "23582", "23583", "23584", "23585", "23586", "23587", "23588", "23589", "23591", "23592", "23593", "23594", "23595", "23596", "23597", "23598", "23599", "23611", "23612", "23613", "23614", "23615", "23616", "23617", "23618", "23619", "23621", "23622", "23623", "23624", "23625", "23626", "23627", "23628", "23629", "23631", "23632", "23633", "23634", "23635", "23636", "23637", "23638", "23639", "23641", "23642", "23643", "23644", "23645", "23646", "23647", "23648", "23649", "23651", "23652", "23653", "23654", "23655", "23656", "23657", "23658", "23659", "23661", "23662", "23663", "23664", "23665", "23666", "23667", "23668", "23669", "23671", "23672", "23673", "23674", "23675", "23676", "23677", "23678", "23679", "23681", "23682", "23683", "23684", "23685", "23686", "23687", "23688", "23689", "23691", "23692", "23693", "23694", "23695", "23696", "23697", "23698", "23699", "23711", "23712", "23713", "23714", "23715", "23716", "23717", "23718", "23719", "23721", "23722", "23723", "23724", "23725", "23726", "23727", "23728", "23729", "23731", "23732", "23733", "23734", "23735", "23736", "23737", "23738", "23739", "23741", "23742", "23743", "23744", "23745", "23746", "23747", "23748", "23749", "23751", "23752", "23753", "23754", "23755", "23756", "23757", "23758", "23759", "23761", "23762", "23763", "23764", "23765", "23766", "23767", "23768", "23769", "23771", "23772", "23773", "23774", "23775", "23776", "23777", "23778", "23779", "23781", "23782", "23783", "23784", "23785", "23786", "23787", "23788", "23789", "23791", "23792", "23793", "23794", "23795", "23796", "23797", "23798", "23799", "23811", "23812", "23813", "23814", "23815", "23816", "23817", "23818", "23819", "23821", "23822", "23823", "23824", "23825", "23826", "23827", "23828", "23829", "23831", "23832", "23833", "23834", "23835", "23836", "23837", "23838", "23839", "23841", "23842", "23843", "23844", "23845", "23846", "23847", "23848", "23849", "23851", "23852", "23853", "23854", "23855", "23856", "23857", "23858", "23859", "23861", "23862", "23863", "23864", "23865", "23866", "23867", "23868", "23869", "23871", "23872", "23873", "23874", "23875", "23876", "23877", "23878", "23879", "23881", "23882", "23883", "23884", "23885", "23886", "23887", "23888", "23889", "23891", "23892", "23893", "23894", "23895", "23896", "23897", "23898", "23899", "23911", "23912", "23913", "23914", "23915", "23916", "23917", "23918", "23919", "23921", "23922", "23923", "23924", "23925", "23926", "23927", "23928", "23929", "23931", "23932", "23933", "23934", "23935", "23936", "23937", "23938", "23939", "23941", "23942", "23943", "23944", "23945", "23946", "23947", "23948", "23949", "23951", "23952", "23953", "23954", "23955", "23956", "23957", "23958", "23959", "23961", "23962", "23963", "23964", "23965", "23966", "23967", "23968", "23969", "23971", "23972", "23973", "23974", "23975", "23976", "23977", "23978", "23979", "23981", "23982", "23983", "23984", "23985", "23986", "23987", "23988", "23989", "23991", "23992", "23993", "23994", "23995", "23996", "23997", "23998", "23999", "44111", "44112", "44113", "44114", "44115", "44116", "44117", "44118", "44119", "44121", "44122", "44123", "44124", "44125", "44126", "44127", "44128", "44129", "44131", "44132", "44133", "44134", "44135", "44136", "44137", "44138", "44139", "44141", "44142", "44143", "44144", "44145", "44146", "44147", "44148", "44149", "44151", "44152", "44153", "44154", "44155", "44156", "44157", "44158", "44159", "44161", "44162", "44163", "44164", "44165", "44166", "44167", "44168", "44169", "44171", "44172", "44173", "44174", "44175", "44176", "44177", "44178", "44179", "44181", "44182", "44183", "44184", "44185", "44186", "44187", "44188", "44189", "44191", "44192", "44193", "44194", "44195", "44196", "44197", "44198", "44199", "44211", "44212", "44213", "44214", "44215", "44216", "44217", "44218", "44219", "44221", "44222", "44223", "44224", "44225", "44226", "44227", "44228", "44229", "44231", "44232", "44233", "44234", "44235", "44236", "44237", "44238", "44239", "44241", "44242", "44243", "44244", "44245", "44246", "44247", "44248", "44249", "44251", "44252", "44253", "44254", "44255", "44256", "44257", "44258", "44259", "44261", "44262", "44263", "44264", "44265", "44266", "44267", "44268", "44269", "44271", "44272", "44273", "44274", "44275", "44276", "44277", "44278", "44279", "44281", "44282", "44283", "44284", "44285", "44286", "44287", "44288", "44289", "44291", "44292", "44293", "44294", "44295", "44296", "44297", "44298", "44299", "44311", "44312", "44313", "44314", "44315", "44316", "44317", "44318", "44319", "44321", "44322", "44323", "44324", "44325", "44326", "44327", "44328", "44329", "44331", "44332", "44333", "44334", "44335", "44336", "44337", "44338", "44339", "44341", "44342", "44343", "44344", "44345", "44346", "44347", "44348", "44349", "44351", "44352", "44353", "44354", "44355", "44356", "44357", "44358", "44359", "44361", "44362", "44363", "44364", "44365", "44366", "44367", "44368", "44369", "44371", "44372", "44373", "44374", "44375", "44376", "44377", "44378", "44379", "44381", "44382", "44383", "44384", "44385", "44386", "44387", "44388", "44389", "44391", "44392", "44393", "44394", "44395", "44396", "44397", "44398", "44399", "44411", "44412", "44413", "44414", "44415", "44416", "44417", "44418", "44419", "44421", "44422", "44423", "44424", "44425", "44426", "44427", "44428", "44429", "44431", "44432", "44433", "44434", "44435", "44436", "44437", "44438", "44439", "44441", "44442", "44443", "44444", "44445", "44446", "44447", "44448", "44449", "44451", "44452", "44453", "44454", "44455", "44456", "44457", "44458", "44459", "44461", "44462", "44463", "44464", "44465", "44466", "44467", "44468", "44469", "44471", "44472", "44473", "44474", "44475", "44476", "44477", "44478", "44479", "44481", "44482", "44483", "44484", "44485", "44486", "44487", "44488", "44489", "44491", "44492", "44493", "44494", "44495", "44496", "44497", "44498", "44499", "44511", "44512", "44513", "44514", "44515", "44516", "44517", "44518", "44519", "44521", "44522", "44523", "44524", "44525", "44526", "44527", "44528", "44529", "44531", "44532", "44533", "44534", "44535", "44536", "44537", "44538", "44539", "44541", "44542", "44543", "44544", "44545", "44546", "44547", "44548", "44549", "44551", "44552", "44553", "44554", "44555", "44556", "44557", "44558", "44559", "44561", "44562", "44563", "44564", "44565", "44566", "44567", "44568", "44569", "44571", "44572", "44573", "44574", "44575", "44576", "44577", "44578", "44579", "44581", "44582", "44583", "44584", "44585", "44586", "44587", "44588", "44589", "44591", "44592", "44593", "44594", "44595", "44596", "44597", "44598", "44599", "44611", "44612", "44613", "44614", "44615", "44616", "44617", "44618", "44619", "44621", "44622", "44623", "44624", "44625", "44626", "44627", "44628", "44629", "44631", "44632", "44633", "44634", "44635", "44636", "44637", "44638", "44639", "44641", "44642", "44643", "44644", "44645", "44646", "44647", "44648", "44649", "44651", "44652", "44653", "44654", "44655", "44656", "44657", "44658", "44659", "44661", "44662", "44663", "44664", "44665", "44666", "44667", "44668", "44669", "44671", "44672", "44673", "44674", "44675", "44676", "44677", "44678", "44679", "44681", "44682", "44683", "44684", "44685", "44686", "44687", "44688", "44689", "44691", "44692", "44693", "44694", "44695", "44696", "44697", "44698", "44699", "44711", "44712", "44713", "44714", "44715", "44716", "44717", "44718", "44719", "44721", "44722", "44723", "44724", "44725", "44726", "44727", "44728", "44729", "44731", "44732", "44733", "44734", "44735", "44736", "44737", "44738", "44739", "44741", "44742", "44743", "44744", "44745", "44746", "44747", "44748", "44749", "44751", "44752", "44753", "44754", "44755", "44756", "44757", "44758", "44759", "44761", "44762", "44763", "44764", "44765", "44766", "44767", "44768", "44769", "44771", "44772", "44773", "44774", "44775", "44776", "44777", "44778", "44779", "44781", "44782", "44783", "44784", "44785", "44786", "44787", "44788", "44789", "44791", "44792", "44793", "44794", "44795", "44796", "44797", "44798", "44799", "44811", "44812", "44813", "44814", "44815", "44816", "44817", "44818", "44819", "44821", "44822", "44823", "44824", "44825", "44826", "44827", "44828", "44829", "44831", "44832", "44833", "44834", "44835", "44836", "44837", "44838", "44839", "44841", "44842", "44843", "44844", "44845", "44846", "44847", "44848", "44849", "44851", "44852", "44853", "44854", "44855", "44856", "44857", "44858", "44859", "44861", "44862", "44863", "44864", "44865", "44866", "44867", "44868", "44869", "44871", "44872", "44873", "44874", "44875", "44876", "44877", "44878", "44879", "44881", "44882", "44883", "44884", "44885", "44886", "44887", "44888", "44889", "44891", "44892", "44893", "44894", "44895", "44896", "44897", "44898", "44899", "44911", "44912", "44913", "44914", "44915", "44916", "44917", "44918", "44919", "44921", "44922", "44923", "44924", "44925", "44926", "44927", "44928", "44929", "44931", "44932", "44933", "44934", "44935", "44936", "44937", "44938", "44939", "44941", "44942", "44943", "44944", "44945", "44946", "44947", "44948", "44949", "44951", "44952", "44953", "44954", "44955", "44956", "44957", "44958", "44959", "44961", "44962", "44963", "44964", "44965", "44966", "44967", "44968", "44969", "44971", "44972", "44973", "44974", "44975", "44976", "44977", "44978", "44979", "44981", "44982", "44983", "44984", "44985", "44986", "44987", "44988", "44989", "44991", "44992", "44993", "44994", "44995", "44996", "44997", "44998", "44999"]

  xit "findPart02" $ do
    (length . findPart02 $ productionProgram) `shouldBe` 3645

  xit "findPart03" $ do
    (length . findPart03 $ productionProgram) `shouldBe` 405

  xit "findPart04" $ do
    (length . findPart04 $ productionProgram) `shouldBe` 10935

  xit "findPart05" $ do
    (length . findPart05 $ productionProgram) `shouldBe` 4860

  xit "findPart06" $ do
    (length . findPart06 $ productionProgram) `shouldBe` 1620

  xit "findPart07" $ do
    (length . findPart07 $ productionProgram) `shouldBe` 8100

  it "hackLargestNumber" $ do
    hackLargestNumber productionInput `shouldBe` "91599994399395"

main = hspec spec

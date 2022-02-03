{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day24 where

-- https://adventofcode.com/2021/day/24

import Data.Char (digitToInt)
import Data.List (group, groupBy, maximumBy, minimumBy, nub, sort, sortBy, sortOn)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, fromList, insert, lookup, toList, union, unionWith)
import Data.Maybe (fromJust)
import Data.Ord
import Prelude hiding (lookup)

type RegisterAddress = Char

data Value = Regular Int | Filter (Int -> Bool)

type Registers = Map RegisterAddress Int

data Token = Register RegisterAddress | Value Int deriving (Eq, Show)

type Tokens = [Token]

type Input = [Char]

type ALU = (Input, Registers)

type Instruction = (String, Tokens)

type Operation = Tokens -> ALU -> ALU

type Operations = Map String Operation

type Program = [Instruction]

parseInput :: String -> Program
parseInput = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction input
  | null tokens = error "parseInstruction: tokens empty!"
  | otherwise = (name, values)
  where
    tokens = splitOn " " input
    name = head tokens :: String
    values = map parseToken . tail $ tokens :: Tokens

parseToken :: String -> Token
parseToken "x" = Register 'x'
parseToken "y" = Register 'y'
parseToken "z" = Register 'z'
parseToken "w" = Register 'w'
parseToken token = Value (read token)

store :: Token -> Int -> Registers -> Registers
store (Register r) value registers = insert r value $ registers
store (Value _) _ _ = error "Not a register address!"

read' :: Token -> Registers -> Int
read' (Register r) registers = fromJust . lookup r $ registers
read' (Value _) _ = error "Not a register address!"

isRegister :: Token -> Bool
isRegister (Register _) = True
isRegister (Value _) = False

isValue :: Token -> Bool
isValue (Value _) = True
isValue (Register _) = False

valueOf :: Token -> Int
valueOf (Value v) = v
valueOf (Register _) = error "Not a value!"

inp :: Tokens -> ALU -> ALU
inp values alu
  | null values = error "inp: values empty!"
  | null input = error "inp: input empty!"
  | otherwise = (tail input, stored)
  where
    (input, registers) = alu
    a = head values :: Token
    value = digitToInt . head $ input :: Int
    stored = store a value registers :: Registers

add :: Tokens -> ALU -> ALU
add = biOp (+)

mul :: Tokens -> ALU -> ALU
mul = biOp (*)

div' :: Tokens -> ALU -> ALU
div' = biOp div

mod' = biOp mod

biOp :: (Int -> Int -> Int) -> Tokens -> ALU -> ALU
biOp op values alu
  | null values = error "biOp: values empty!"
  | otherwise = (input, stored)
  where
    (input, registers) = alu
    a = head values :: Token
    b = head . tail $ values :: Token
    x = read' a registers :: Int
    y
      | isRegister b = read' b registers
      | otherwise = valueOf b
    result = x `op` y :: Int
    stored = store a result registers :: Registers

eql :: Tokens -> ALU -> ALU
eql = biOp equal
  where
    equal :: Int -> Int -> Int
    equal a b
      | a == b = 1
      | otherwise = 0

emptyRegisters :: Registers
emptyRegisters =
  fromList
    [ ('x', 0),
      ('y', 0),
      ('z', 0),
      ('w', 0)
    ]

registersOf :: ALU -> Registers
registersOf (_, registers) = registers

operations =
  fromList
    [ ("inp", inp),
      ("add", add),
      ("mul", mul),
      ("div", div'),
      ("mod", mod'),
      ("eql", eql)
    ] ::
    Operations

operate :: ALU -> Instruction -> ALU
operate alu instruction = op values alu
  where
    (name, values) = instruction
    op = fromJust . lookup name $ operations :: Operation

process :: Input -> Program -> Registers
process input = registersOf . foldl operate alu
  where
    alu = (input, emptyRegisters) :: ALU

validationCode :: Input -> Program -> Int
validationCode input = fromJust . lookup 'z' . process input

splitIntoSubPrograms :: Program -> [Program]
splitIntoSubPrograms [] = []
splitIntoSubPrograms programs = program : splitIntoSubPrograms others
  where
    program = head programs : (takeWhile (\(name, _) -> name /= "inp") . tail $ programs)
    others = dropWhile (\(name, _) -> name /= "inp") . tail $ programs

processZ :: Program -> String -> [Int]
processZ program = processThrough (splitIntoSubPrograms program)

processThrough :: [Program] -> String -> [Int]
processThrough programs number = map (validationCode number) usePrograms
  where
    size = length number :: Int
    usePrograms = map (concat . (`take` programs)) [1 .. size] :: [Program]

allNumbers :: Int -> [String]
allNumbers size = filter ('0' `notElem`) . map show $ [min .. max]
  where
    max = read . replicate size $ '9' :: Integer
    min = read . replicate size $ '1' :: Integer

findPart01 :: Program -> [String]
findPart01 program =
  map fst
    . filter (\(_, one : two : three : four : five : _) -> three == five)
    . map (\n -> (n, processZ program n))
    $ numbers
  where
    size = 5
    numbers = allNumbers size :: [String]

extendNumbers :: Int -> [String] -> [String]
extendNumbers nDigits = concatMap (\n -> map (n ++) extensions)
  where
    extensions = allNumbers nDigits :: [String]

findPart02 :: Program -> [String]
findPart02 program =
  map fst
    . filter (\(_, one : two : three : four : five : six : _) -> two == six)
    . map (\n -> (n, processZ program n))
    $ extended
  where
    part01 = findPart01 program :: [String]
    extended = extendNumbers 1 part01 :: [String]

findPart03 :: Program -> [String]
findPart03 program =
  map fst
    . filter (\(_, one : two : three : four : five : six : seven : _) -> one == seven)
    . map (\n -> (n, processZ program n))
    $ extended
  where
    part02 = findPart02 program :: [String]
    extended = extendNumbers 1 part02 :: [String]

findPart04 :: Program -> [String]
findPart04 program =
  map fst
    . filter (\(_, one : two : three : four : five : six : seven : eight : nine : ten : _) -> eight == ten)
    . map (\n -> (n, processZ program n))
    $ extended
  where
    part03 = findPart03 program :: [String]
    extended = extendNumbers 3 part03 :: [String]

findPart05 :: Program -> [String]
findPart05 program =
  map fst
    . filter (\(_, one : two : three : four : five : six : seven : eight : nine : ten : eleven : _) -> eleven == seven)
    . map (\n -> (n, processZ program n))
    $ extended
  where
    part04 = findPart04 program :: [String]
    extended = extendNumbers 1 part04 :: [String]

findPart06 :: Program -> [String]
findPart06 program =
  map fst
    . filter (\(_, one : two : three : four : five : six : seven : eight : nine : ten : eleven : twelve : _) -> twelve == 0)
    . map (\n -> (n, processZ program n))
    $ extended
  where
    part05 = findPart05 program :: [String]
    extended = extendNumbers 1 part05 :: [String]

findPart07 :: Program -> [String]
findPart07 program =
  map fst
    . filter (\(_, one : two : three : four : five : six : seven : eight : nine : ten : eleven : twelve : thirdteen : fourthteen : _) -> fourthteen == 0)
    . map (\n -> (n, processZ program n))
    $ extended
  where
    part06 = findPart06 program :: [String]
    extended = extendNumbers 2 part06 :: [String]

hackLargestNumber :: String -> String
hackLargestNumber = last . findPart07 . parseInput

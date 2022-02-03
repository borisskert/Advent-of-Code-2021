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

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn function = groupBy (\a b -> function a == function b)

replaceDigitAt :: Int -> Char -> String -> String
replaceDigitAt index digit number = (++ end) . (++ [digit]) $ start
  where
    start = take index number :: String
    end = tail . drop index $ number :: String

createNumbers :: Int -> String -> [(Char, String)]
createNumbers index number = map (\digit -> (digit, replaceDigitAt index digit number)) digits
  where
    digits = reverse "123456789" :: [Char]

numberGroupsAt :: Int -> Program -> [String]
numberGroupsAt i program = bla
  where
    start = replicate 14 $ '9' :: String
    numbers = createNumbers i start :: [(Char, String)]
    bla = map (map fst) . groupOn snd . sortOn (Down . snd) . map (\(digit, number) -> (digit, validationCode number program)) $ numbers :: [[Char]]

numberGroups :: Program -> [[String]]
numberGroups program = map (reverse . sort . (`numberGroupsAt` program)) [0 .. 13]

makeNumbers :: [[[Char]]] -> [String]
makeNumbers [] = [""]
makeNumbers (g : gs) = concatMap (\d -> map (d :) next) digits
  where
    digits = map head g :: [Char]
    next = makeNumbers gs :: [String]

findFirstValidNumber :: Program -> [String] -> String
findFirstValidNumber program = head . map fst . filter ((== 0) . snd) . map (\n -> (n, validationCode n program))

hackLargest :: Program -> String
hackLargest program = findFirstValidNumber program numbers
  where
    groups = numberGroups program
    numbers = makeNumbers groups :: [String]

hackLargestNumber :: String -> String
hackLargestNumber = hackLargest . parseInput

module Day16 where

-- https://adventofcode.com/2021/day/16

import Data.Either (Either (Left, Right))
import Data.Maybe (fromJust, isNothing)
import Prelude hiding (Left, Right)

readBitsFromString :: String -> [Bool]
readBitsFromString = concatMap toBits
  where
    toBits :: Char -> [Bool]
    toBits '0' = [False, False, False, False]
    toBits '1' = [False, False, False, True]
    toBits '2' = [False, False, True, False]
    toBits '3' = [False, False, True, True]
    toBits '4' = [False, True, False, False]
    toBits '5' = [False, True, False, True]
    toBits '6' = [False, True, True, False]
    toBits '7' = [False, True, True, True]
    toBits '8' = [True, False, False, False]
    toBits '9' = [True, False, False, True]
    toBits 'A' = [True, False, True, False]
    toBits 'B' = [True, False, True, True]
    toBits 'C' = [True, True, False, False]
    toBits 'D' = [True, True, False, True]
    toBits 'E' = [True, True, True, False]
    toBits 'F' = [True, True, True, True]
    toBits c = error ("Unknown char: " ++ [c])

convertIntFromBits :: [Bool] -> Int
convertIntFromBits [] = 0
convertIntFromBits b = convertIntFromBits (init b) * 2 + payload
  where
    payload = if last b then 1 else 0

data Version = Version Int deriving (Eq, Show)

versionBitSize :: Int
versionBitSize = 3

readVersionFromBits :: [Bool] -> Version
readVersionFromBits = Version . convertIntFromBits . take versionBitSize

versionValue :: Version -> Int
versionValue (Version value) = value

data TypeId = TypeId Int deriving (Eq, Show)

typeIdBitSize :: Int
typeIdBitSize = 3

readTypeIdFromBits :: [Bool] -> TypeId
readTypeIdFromBits = TypeId . convertIntFromBits . take typeIdBitSize . drop versionBitSize

isLiteralTypeId :: TypeId -> Bool
isLiteralTypeId (TypeId 4) = True
isLiteralTypeId (TypeId _) = False

data Literal = Literal (Version, TypeId, [Group]) deriving (Eq, Show)

readLiteral :: String -> Literal
readLiteral = readLiteralFromBits . readBitsFromString

groupBitSize :: Int
groupBitSize = 5 :: Int

groupsBitSize :: [Group] -> Int
groupsBitSize = (* groupBitSize) . length

expectedLiteralBitSize :: Literal -> Int
expectedLiteralBitSize literal
  | snd quattro == 0 = expectedSize
  | otherwise = (* 4) . (+ 1) . fst $ quattro
  where
    expectedSize = actualLiteralBitSize literal :: Int
    quattro = divMod expectedSize 4 :: (Int, Int)

actualLiteralBitSize :: Literal -> Int
actualLiteralBitSize (Literal (_, _, groups)) = expectedSize
  where
    expectedSize = versionBitSize + typeIdBitSize + groupsBitSize groups :: Int
    quattro = divMod expectedSize 4 :: (Int, Int)

minimalLiteralBitSize :: Int
minimalLiteralBitSize = versionBitSize + typeIdBitSize + groupBitSize

tryReadLiteralFromBits :: [Bool] -> Maybe Literal
tryReadLiteralFromBits bits
  | length bits < minimalLiteralBitSize = Nothing
  | otherwise = Just . readLiteralFromBits $ bits

data Group = Group (Bool, Int) deriving (Eq, Show)

readGroupFromBits :: [Bool] -> Group
readGroupFromBits bits = Group (isLast, payload)
  where
    groupPayloadBitSize = 4 :: Int
    isLast = (== False) . head $ bits :: Bool
    payload = convertIntFromBits . take groupPayloadBitSize . tail $ bits :: Int

isLastGroup :: Group -> Bool
isLastGroup (Group (isLast, _readToVersion)) = isLast

readGroupsFromBits :: [Bool] -> [Group]
readGroupsFromBits [] = []
readGroupsFromBits bits
  | isLastGroup readGroup = [readGroup]
  | otherwise = readGroup : readGroupsFromBits furtherBits
  where
    readGroup = readGroupFromBits bits :: Group
    furtherBits = drop groupBitSize bits :: [Bool]

groupToPayload :: Group -> Int
groupToPayload (Group (_, payload)) = payload

groupsToPayload :: [Group] -> Int
groupsToPayload [] = 0
groupsToPayload gs = payload + 16 * furtherPayload
  where
    payload = groupToPayload . last $ gs :: Int
    furtherPayload = groupsToPayload (init gs) :: Int

readLiteralFromBits :: [Bool] -> Literal
readLiteralFromBits bits = Literal (version, typeId, groups)
  where
    version = readVersionFromBits bits :: Version
    typeId = readTypeIdFromBits bits :: TypeId
    droppedHeader = drop typeIdBitSize . drop versionBitSize $ bits :: [Bool]
    groups = readGroupsFromBits droppedHeader :: [Group]

data LengthType = LengthType Int deriving (Eq, Show)

lengthTypeBitSize :: Int
lengthTypeBitSize = 1

readLengthTypeFromBits :: [Bool] -> LengthType
readLengthTypeFromBits = LengthType . convertIntFromBits . take lengthTypeBitSize . drop typeIdBitSize . drop versionBitSize

data SubPacketsLength = SubPacketsLength Int deriving (Eq, Show)

subPacketsLengthBitSize :: Int
subPacketsLengthBitSize = 15

data SubPacketsCount = SubPacketsCount Int deriving (Eq, Show)

subPacketsCountBitSize :: Int
subPacketsCountBitSize = 11

type SubPacketsLengthOrCount = Either SubPacketsLength SubPacketsCount

readSubPacketsLengthFromBits :: [Bool] -> SubPacketsLengthOrCount
readSubPacketsLengthFromBits bits
  | lengthType == LengthType 0 = Left . SubPacketsLength . convertIntFromBits . take subPacketsLengthBitSize . drop lengthTypeBitSize . drop typeIdBitSize . drop versionBitSize $ bits
  | otherwise = Right . SubPacketsCount . convertIntFromBits . take subPacketsCountBitSize . drop lengthTypeBitSize . drop typeIdBitSize . drop versionBitSize $ bits
  where
    lengthType = readLengthTypeFromBits bits :: LengthType

isSubPacketsLength :: SubPacketsLengthOrCount -> Bool
isSubPacketsLength (Left (SubPacketsLength _)) = True
isSubPacketsLength (Right (SubPacketsCount _)) = False

subPacketLengthOrCountValue :: SubPacketsLengthOrCount -> Int
subPacketLengthOrCountValue (Left (SubPacketsLength x)) = x
subPacketLengthOrCountValue (Right (SubPacketsCount x)) = x

subPacketsLengthOrCountBitSize :: SubPacketsLengthOrCount -> Int
subPacketsLengthOrCountBitSize (Left (SubPacketsLength _)) = subPacketsLengthBitSize
subPacketsLengthOrCountBitSize (Right (SubPacketsCount _)) = subPacketsCountBitSize

data Operator = Operator (Version, TypeId, LengthType, SubPacketsLengthOrCount, [Package]) deriving (Eq, Show)

minimalOperatorBitSize :: Int
minimalOperatorBitSize = versionBitSize + typeIdBitSize + lengthTypeBitSize + subPacketsCountBitSize

readOperatorFromBits :: [Bool] -> Operator
readOperatorFromBits bits
  | isSubPacketsLength subpackets = readWithSubPacketsLength
  | otherwise = readWithSubPacketsCount
  where
    version = readVersionFromBits bits :: Version
    typeId = readTypeIdFromBits bits :: TypeId
    lengthType = readLengthTypeFromBits bits :: LengthType
    subpackets = readSubPacketsLengthFromBits bits :: SubPacketsLengthOrCount

    readWithSubPacketsLength :: Operator
    readWithSubPacketsLength = Operator (version, typeId, lengthType, subpackets, readSubPackages)
      where
        subpacketsBitSize = subPacketLengthOrCountValue subpackets :: Int
        subpacketsBits = take subpacketsBitSize . drop subPacketsLengthBitSize . drop lengthTypeBitSize . drop typeIdBitSize . drop versionBitSize $ bits
        readSubPackages = readPackagesFromBits subpacketsBits :: [Package]

    readWithSubPacketsCount :: Operator
    readWithSubPacketsCount = Operator (version, typeId, lengthType, subpackets, readSubPackages)
      where
        subpacketsCount = subPacketLengthOrCountValue subpackets :: Int
        droppedHeader = drop subPacketsCountBitSize . drop lengthTypeBitSize . drop typeIdBitSize . drop versionBitSize $ bits :: [Bool]
        readSubPackages = readNPackagesFromBits subpacketsCount droppedHeader :: [Package]

tryReadOperatorFromBits :: [Bool] -> Maybe Operator
tryReadOperatorFromBits bits
  | length bits < minimalOperatorBitSize = Nothing
  | otherwise = Just . readOperatorFromBits $ bits

type Package = Either Literal Operator

expectedOperatorBitSize :: Operator -> Int
expectedOperatorBitSize (Operator (_, _, lengthType, subpackets, subpackages)) = expectedSize
  where
    header = versionBitSize + typeIdBitSize + lengthTypeBitSize + subpackagesLenthOrCountSize
    expectedSize = (header +) . sum . map expectedBitSize $ subpackages
    subpackagesLenthOrCountSize
      | isSubPacketsLength subpackets = subPacketsLengthBitSize
      | otherwise = subPacketsCountBitSize

actualOperatorBitSize :: Operator -> Int
actualOperatorBitSize (Operator (_, _, lengthType, subpackets, subpackages)) = expectedSize
  where
    header = versionBitSize + typeIdBitSize + lengthTypeBitSize + subpackagesLenthOrCountSize
    expectedSize = (header +) . sum . map actualBitSize $ subpackages
    subpackagesLenthOrCountSize
      | isSubPacketsLength subpackets = subPacketsLengthBitSize
      | otherwise = subPacketsCountBitSize

expectedBitSize :: Package -> Int
expectedBitSize (Left literal) = expectedLiteralBitSize literal
expectedBitSize (Right operator) = expectedOperatorBitSize operator

actualBitSize :: Package -> Int
actualBitSize (Left literal) = actualLiteralBitSize literal
actualBitSize (Right operator) = actualOperatorBitSize operator

readPackage :: String -> Package
readPackage = readPackageFromBits . readBitsFromString

readNPackagesFromBits :: Int -> [Bool] -> [Package]
readNPackagesFromBits 0 _ = []
readNPackagesFromBits n bits = readPackage : readNPackagesFromBits (n - 1) followingBits
  where
    readPackage = readPackageFromBits bits :: Package
    packageBitSize = actualBitSize readPackage :: Int
    followingBits = drop packageBitSize bits :: [Bool]

readPackages :: String -> [Package]
readPackages = readPackagesFromBits . readBitsFromString

readPackageFromBits :: [Bool] -> Package
readPackageFromBits bits
  | isLiteralTypeId typeId = Left . readLiteralFromBits $ bits
  | otherwise = Right . readOperatorFromBits $ bits
  where
    typeId = readTypeIdFromBits bits

tryReadPackageFromBits :: [Bool] -> Maybe Package
tryReadPackageFromBits bits
  | isLiteralTypeId typeId = fmap Left . tryReadLiteralFromBits $ bits
  | otherwise = fmap Right . tryReadOperatorFromBits $ bits
  where
    typeId = readTypeIdFromBits bits

readPackagesFromBits :: [Bool] -> [Package]
readPackagesFromBits bits
  | isNothing maybePackage = []
  | otherwise = readPackage : readPackagesFromBits followingBits
  where
    maybePackage = tryReadPackageFromBits bits :: Maybe Package
    readPackage = fromJust maybePackage :: Package
    packageBitSize = actualBitSize readPackage :: Int
    followingBits = drop packageBitSize bits :: [Bool]

literalPackageVersionValue :: Literal -> Int
literalPackageVersionValue (Literal (version, _, _)) = versionValue version

operatorPackageVersionValue :: Operator -> Int
operatorPackageVersionValue (Operator (version, _, _, _, subpackets)) = versionValue version + subpacketVersionValue
  where
    subpacketVersionValue = packagesVersionValue subpackets :: Int

packageVersionValue :: Package -> Int
packageVersionValue (Left literal) = literalPackageVersionValue literal
packageVersionValue (Right operator) = operatorPackageVersionValue operator

packagesVersionValue :: [Package] -> Int
packagesVersionValue = sum . map packageVersionValue

versionSum :: String -> Int
versionSum = packagesVersionValue . readPackages

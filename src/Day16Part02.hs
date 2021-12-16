module Day16Part02 where

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

readLiteral :: String -> Package
readLiteral = readLiteralFromBits . readBitsFromString

groupBitSize :: Int
groupBitSize = 5 :: Int

groupsBitSize :: [Group] -> Int
groupsBitSize = (* groupBitSize) . length

minimalLiteralBitSize :: Int
minimalLiteralBitSize = versionBitSize + typeIdBitSize + groupBitSize

tryReadLiteralFromBits :: [Bool] -> Maybe Package
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

readLiteralFromBits :: [Bool] -> Package
readLiteralFromBits bits = Literal (version, groups)
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

minimalOperatorBitSize :: Int
minimalOperatorBitSize = versionBitSize + typeIdBitSize + lengthTypeBitSize + subPacketsCountBitSize

createOperator :: Version -> TypeId -> LengthType -> SubPacketsLengthOrCount -> [Package] -> Package
createOperator version (TypeId 0) lengthType subpacketsize subpackages = Sum (version, lengthType, subpacketsize, subpackages)
createOperator version (TypeId 1) lengthType subpacketsize subpackages = Product (version, lengthType, subpacketsize, subpackages)
createOperator version (TypeId 2) lengthType subpacketsize subpackages = Minimum (version, lengthType, subpacketsize, subpackages)
createOperator version (TypeId 3) lengthType subpacketsize subpackages = Maximum (version, lengthType, subpacketsize, subpackages)
createOperator version (TypeId 5) lengthType subpacketsize subpackages = GreaterThan (version, lengthType, subpacketsize, subpackages)
createOperator version (TypeId 6) lengthType subpacketsize subpackages = LessThan (version, lengthType, subpacketsize, subpackages)
createOperator version (TypeId 7) lengthType subpacketsize subpackages = EqualTo (version, lengthType, subpacketsize, subpackages)

readOperatorFromBits :: [Bool] -> Package
readOperatorFromBits bits
  | isSubPacketsLength subpackets = readWithSubPacketsLength
  | otherwise = readWithSubPacketsCount
  where
    version = readVersionFromBits bits :: Version
    typeId = readTypeIdFromBits bits :: TypeId
    lengthType = readLengthTypeFromBits bits :: LengthType
    subpackets = readSubPacketsLengthFromBits bits :: SubPacketsLengthOrCount

    readWithSubPacketsLength :: Package
    readWithSubPacketsLength = createOperator version typeId lengthType subpackets readSubPackages
      where
        subpacketsBitSize = subPacketLengthOrCountValue subpackets :: Int
        subpacketsBits = take subpacketsBitSize . drop subPacketsLengthBitSize . drop lengthTypeBitSize . drop typeIdBitSize . drop versionBitSize $ bits
        readSubPackages = readPackagesFromBits subpacketsBits :: [Package]

    readWithSubPacketsCount :: Package
    readWithSubPacketsCount = createOperator version typeId lengthType subpackets readSubPackages
      where
        subpacketsCount = subPacketLengthOrCountValue subpackets :: Int
        droppedHeader = drop subPacketsCountBitSize . drop lengthTypeBitSize . drop typeIdBitSize . drop versionBitSize $ bits :: [Bool]
        readSubPackages = readNPackagesFromBits subpacketsCount droppedHeader :: [Package]

tryReadOperatorFromBits :: [Bool] -> Maybe Package
tryReadOperatorFromBits bits
  | length bits < minimalOperatorBitSize = Nothing
  | otherwise = Just . readOperatorFromBits $ bits

data Package
  = Literal (Version, [Group])
  | Sum (Version, LengthType, SubPacketsLengthOrCount, [Package])
  | Product (Version, LengthType, SubPacketsLengthOrCount, [Package])
  | Minimum (Version, LengthType, SubPacketsLengthOrCount, [Package])
  | Maximum (Version, LengthType, SubPacketsLengthOrCount, [Package])
  | GreaterThan (Version, LengthType, SubPacketsLengthOrCount, [Package])
  | LessThan (Version, LengthType, SubPacketsLengthOrCount, [Package])
  | EqualTo (Version, LengthType, SubPacketsLengthOrCount, [Package])
  deriving (Eq, Show)

bitSize :: Package -> Int
bitSize (Literal (_, groups)) = expectedSize
  where
    expectedSize = versionBitSize + typeIdBitSize + groupsBitSize groups :: Int
    quattro = divMod expectedSize 4 :: (Int, Int)
bitSize (Sum (_, lengthType, subpackets, subpackages)) = bitSizeOf lengthType subpackets subpackages
bitSize (Product (_, lengthType, subpackets, subpackages)) = bitSizeOf lengthType subpackets subpackages
bitSize (Minimum (_, lengthType, subpackets, subpackages)) = bitSizeOf lengthType subpackets subpackages
bitSize (Maximum (_, lengthType, subpackets, subpackages)) = bitSizeOf lengthType subpackets subpackages
bitSize (GreaterThan (_, lengthType, subpackets, subpackages)) = bitSizeOf lengthType subpackets subpackages
bitSize (LessThan (_, lengthType, subpackets, subpackages)) = bitSizeOf lengthType subpackets subpackages
bitSize (EqualTo (_, lengthType, subpackets, subpackages)) = bitSizeOf lengthType subpackets subpackages

bitSizeOf :: LengthType -> SubPacketsLengthOrCount -> [Package] -> Int
bitSizeOf lengthType subpackets subpackages = expectedSize
  where
    header = versionBitSize + typeIdBitSize + lengthTypeBitSize + subpackagesLenthOrCountSize
    expectedSize = (header +) . sum . map bitSize $ subpackages
    subpackagesLenthOrCountSize
      | isSubPacketsLength subpackets = subPacketsLengthBitSize
      | otherwise = subPacketsCountBitSize

readPackage :: String -> Package
readPackage = readPackageFromBits . readBitsFromString

readNPackagesFromBits :: Int -> [Bool] -> [Package]
readNPackagesFromBits 0 _ = []
readNPackagesFromBits n bits = readPackage : readNPackagesFromBits (n - 1) followingBits
  where
    readPackage = readPackageFromBits bits :: Package
    packageBitSize = bitSize readPackage :: Int
    followingBits = drop packageBitSize bits :: [Bool]

readPackages :: String -> [Package]
readPackages = readPackagesFromBits . readBitsFromString

readPackageFromBits :: [Bool] -> Package
readPackageFromBits bits
  | isLiteralTypeId typeId = readLiteralFromBits $ bits
  | otherwise = readOperatorFromBits $ bits
  where
    typeId = readTypeIdFromBits bits

tryReadPackageFromBits :: [Bool] -> Maybe Package
tryReadPackageFromBits bits
  | isLiteralTypeId typeId = tryReadLiteralFromBits bits
  | otherwise = tryReadOperatorFromBits bits
  where
    typeId = readTypeIdFromBits bits

readPackagesFromBits :: [Bool] -> [Package]
readPackagesFromBits bits
  | isNothing maybePackage = []
  | otherwise = readPackage : readPackagesFromBits followingBits
  where
    maybePackage = tryReadPackageFromBits bits :: Maybe Package
    readPackage = fromJust maybePackage :: Package
    packageBitSize = bitSize readPackage :: Int
    followingBits = drop packageBitSize bits :: [Bool]

evaluateBit :: Bool -> Int
evaluateBit True = 1
evaluateBit False = 0

evalulatePackage :: Package -> Int
evalulatePackage (Literal (_, groups)) = groupsToPayload groups
evalulatePackage (Sum (_, lengthType, subpackets, subpackages)) = sum . map evalulatePackage $ subpackages
evalulatePackage (Product (_, lengthType, subpackets, subpackages)) = product . map evalulatePackage $ subpackages
evalulatePackage (Minimum (_, lengthType, subpackets, subpackages)) = minimum . map evalulatePackage $ subpackages
evalulatePackage (Maximum (_, lengthType, subpackets, subpackages)) = maximum . map evalulatePackage $ subpackages
evalulatePackage (GreaterThan (_, lengthType, subpackets, subpackages)) = evaluateBit . ((evalulatePackage . head . tail $ subpackages) <) . evalulatePackage . head $ subpackages
evalulatePackage (LessThan (_, lengthType, subpackets, subpackages)) = evaluateBit . ((evalulatePackage . head . tail $ subpackages) >) . evalulatePackage . head $ subpackages
evalulatePackage (EqualTo (_, lengthType, subpackets, subpackages)) = evaluateBit . ((evalulatePackage . head . tail $ subpackages) ==) . evalulatePackage . head $ subpackages

evaluate :: String -> Int
evaluate = evalulatePackage . head . readPackages

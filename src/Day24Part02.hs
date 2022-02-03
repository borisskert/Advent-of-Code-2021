module Day24Part02 where

import Data.List (minimumBy, nub, partition, sort, sortOn, (\\))
import Data.Maybe
import Data.Ord
import Day24 (Program, findPart07, parseInput, validationCode)

hackLowestNumber :: String -> String
hackLowestNumber = head . findPart07 . parseInput

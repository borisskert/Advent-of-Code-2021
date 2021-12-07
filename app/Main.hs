module Main where

import Days (runDays)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  runDays args

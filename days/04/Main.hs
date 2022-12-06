module Main
  ( main
  ) where

import           Data.List       (intersect)
import           Data.List.Split (splitOneOf)

import           Lib

main = runDay partOne partTwo

partOne = length . filter (overlap True) . map parseLine

partTwo = length . filter (overlap False) . map parseLine

parseLine = (map read) . splitOneOf "-,"

overlap :: Bool -> [Int] -> Bool
overlap full [a, b, x, y] =
  case full of
    True  -> length common > 0
    False -> elem common [one, two]
  where
    one = [a .. b]
    two = [x .. y]
    common = intersect one two

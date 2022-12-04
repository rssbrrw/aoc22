module Main (main) where

import Data.List       (sort)
import Data.List.Split (splitOn)

import Lib

main = runDay partOne partTwo

partOne = maximum . elves
    
partTwo = sum . take 3 . reverse . sort . elves

elves :: [String] -> [Int]
elves input = map sum sacks
    where
        sacks = map (map read) $ splitOn [""] input
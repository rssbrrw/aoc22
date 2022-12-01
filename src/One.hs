module One (
    solutionOne,
    solutionTwo
)
    where

import Data.List (sort)
import Data.List.Split (splitOn)

solutionOne :: [String] -> Int
solutionOne = maximum . elves
    
solutionTwo :: [String] -> Int
solutionTwo = sum . take 3 . reverse . sort . elves

elves :: [String] -> [Int]
elves input = map sum sacks
    where
        sacks = map (map read) $ splitOn [""] input
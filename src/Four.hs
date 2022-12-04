module Four (
    dayNo,
    solutionOne,
    solutionTwo
) where

import Data.List
import Data.List.Split

dayNo = 4

solutionOne = length 
            . filter (overlap True)
            . map parseLine
        
solutionTwo = length
            . filter (overlap False)
            . map parseLine

parseLine = (map read) . splitOneOf "-,"

overlap :: Bool -> [Int] -> Bool
overlap full [a, b, x, y] = case full of
    True  -> length common > 0
    False -> elem common [one, two]
    where
        one    = [a..b]
        two    = [x..y]
        common = intersect one two
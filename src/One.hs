module One (
    solutionOne,
    solutionTwo
)
    where

import Data.List (sort)
import Data.List.Split (splitOn)

solutionOne :: String -> Int
solutionOne input = maximum $ mapSum input
    
solutionTwo :: String -> Int
solutionTwo input = sum $ take 3 $ reverse $ sort $ mapSum input

readAll :: [String] -> [Int]
readAll xs = map (\x -> read x :: Int) xs

mapSum :: String -> [Int]
mapSum input = map sum $ map readAll $ splitOn [""] $ splitOn "\n" input
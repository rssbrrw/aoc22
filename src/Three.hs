module Three (
    dayNo,
    solutionOne,
    solutionTwo
) where

import Data.List (elemIndex, intersect)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

dayNo = 3

solutionOne :: [String] -> Int
solutionOne = sum . map (priority . commonItem . splitItems)

solutionTwo :: [String] -> Int
solutionTwo = sum . map (priority . commonItem) . chunksOf 3

commonItem :: [String] -> Char
commonItem (x:xs) = (foldl (flip intersect) x xs) !! 0

priority :: Char -> Int
priority c = (+1) . fromJust $ elemIndex c (['a'..'z'] ++ ['A'..'Z'])

splitItems :: String -> [String]
splitItems s = [take half s, drop half s]
    where half = length s `div` 2
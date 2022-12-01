module Main (main) where

import One

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solutionOne input
    print $ solutionTwo input

module Main (main) where

import One

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print $ solutionOne input
    print $ solutionTwo input

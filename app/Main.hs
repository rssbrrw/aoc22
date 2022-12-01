module Main (main) where


import System.Environment (getArgs)

import One

main :: IO ()
main = do
    dayNo <- head <$> getArgs
    input <- lines <$> readFile ("input" ++ dayNo ++ ".txt")
    print $ solutionOne input
    print $ solutionTwo input
